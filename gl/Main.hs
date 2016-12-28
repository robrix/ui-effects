{-# LANGUAGE DataKinds, FlexibleContexts, RankNTypes #-}
module Main where

import Control.Exception
import Control.Monad
import Control.Monad.Effect
import Control.Monad.Effect.Internal
import qualified Control.Monad.Effect.State as State
import Control.Monad.IO.Class
import GL.Draw
import GL.Exception
import GL.Geometry
import GL.Program
import GL.Scalar
import GL.Shader
import GL.Setup hiding (Shader)
import qualified Linear.Affine as Linear
import qualified Linear.Matrix as Linear
import qualified Linear.V2 as Linear
import qualified Linear.V4 as Linear
import Prelude hiding (IO)
import qualified Prelude
import SDL.Event
import SDL.Init
import System.Exit
import UI.Drawing
import UI.Geometry
import UI.Interaction
import UI.View
import UI.Window

main :: IO ()
main = runWindow "UI" (runSetup . setup)
  `catch`
    (putStrLn . displayException :: SomeException -> IO ())
  `finally`
    exitSuccess

rectGeometry :: GLScalar a => Rect a -> Geometry (Linear.V4 a)
rectGeometry (Rect (Point x y) (Size w h)) = Geometry TriangleStrip
  [ Linear.V4 x        y      0 1
  , Linear.V4 x       (y + h) 0 1
  , Linear.V4 (x + w)  y      0 1
  , Linear.V4 (x + w) (y + h) 0 1 ]

setup :: IO () -> Setup a
setup swap = do
  enable DepthTest
  enable Blending
  setDepthFunc Always
  setBlendFactors SourceAlpha OneMinusSourceAlpha
  setClearColour (rgba 0 0 0 1)
  matrix <- uniform
  xy <- uniform
  let vertexShader = toShader (\ p -> pure (vertex { position = get matrix !* get p }) :: Shader Vertex)
  let fragmentShader = get xy
  program <- buildProgram [ Vertex vertexShader, Fragment fragmentShader ]
  array <- geometry (rectGeometry <$> renderingBackgroundRects (renderView view :: Rendering Float (Size Float)))
  liftIO (runIOState (Linear.V2 512 384 :: Linear.V2 Float) . forever $ do
    event <- send (waitEvent :: Prelude.IO Event)
    case eventPayload event of
      MouseMotionEvent m -> do
        let Linear.P p = fromIntegral <$> mouseMotionEventPos m :: Linear.Point Linear.V2 Float
        State.put p
      QuitEvent -> do
        sendVoid quit
        sendVoid exitSuccess
      _ -> pure ()
    sendVoid $ runInteraction event (clickable (Rect (Point 0 0) (Size 100 100) :: Rect Int) (pure ()))
    pos <- State.get
    sendVoid $ runDraw (draw matrix xy pos program array)
    sendVoid swap)
  where sendVoid io = send (io :: Prelude.IO ())

draw :: Var (Shader (Linear.M44 Float)) -> Var (Shader (Linear.V4 Float)) -> Linear.V2 Float -> GLProgram -> GeometryArray Float -> Draw ()
draw matrix xy (Linear.V2 x y) program array = do
  clear [ ColourBuffer, DepthBuffer ]

  useProgram program

  setUniform program xy (Linear.V4 (x / 1024) (y / 768) 1 0.5)
  setUniform program matrix (orthographic 0 1024 0 768 (negate 1) 1)

  drawGeometry array

view :: View
view = list
  [ label "hello, world"
  , label "what’s up?" ]

orthographic :: Fractional a => a -> a -> a -> a -> a -> a -> Linear.M44 a
orthographic left right top bottom near far = Linear.V4
  (Linear.V4 (2 / (right - left))  0                    0                         tx)
  (Linear.V4  0                   (2 / (top - bottom))  0                         ty)
  (Linear.V4  0                    0                   (negate 2 / (far - near))  tz)
  (Linear.V4  0                    0                    0                         1)
  where tx = negate ((right + left) / (right - left))
        ty = negate ((top + bottom) / (top - bottom))
        tz = negate ((far + near) / (far - near))

type IOState s a = Eff '[State.State s, Prelude.IO] a

runIOState :: s -> IOState s a -> Prelude.IO a
runIOState s = runM . fmap fst . flip State.runState s
