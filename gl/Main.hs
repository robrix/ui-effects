{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Exception
import Control.Monad
import Data.Time.Clock.POSIX
import GL.Array
import GL.Draw
import GL.Exception
import GL.Program
import GL.Shader
import GL.Setup
import qualified Linear.V2 as Linear
import qualified Linear.V4 as Linear
import Prelude hiding (IO)
import System.Exit
import UI.Drawing
import UI.Geometry
import UI.View
import UI.Window

main :: IO ()
main = runWindow "UI" (\ swap ->
  runSetup (setup (\ state -> forever (runDraw (uncurry draw state) >> swap))))
  `catch`
    (putStrLn . displayException :: SomeException -> IO ())
  `finally`
    exitSuccess

vertices :: Num n => Shape n -> [Linear.V4 n]
vertices (Rectangle (Linear.V2 ax ay) (Linear.V2 bx by)) =
  [ Linear.V4 ax ay 0 1
  , Linear.V4 ax by 0 1
  , Linear.V4 bx ay 0 1
  , Linear.V4 bx by 0 1
  ]

rectVertices :: Num a => Rect a -> [Linear.V4 a]
rectVertices (Rect (Point x y) (Size w h)) =
  [ Linear.V4 x        y      0 1
  , Linear.V4 x       (y + h) 0 1
  , Linear.V4 (x + w)  y      0 1
  , Linear.V4 (x + w) (y + h) 0 1
  ]

setup :: ((GLProgram, GLArray Float) -> IO a) -> Setup a
setup f = do
  _ <- enable DepthTest
  _ <- setDepthFunc Less
  _ <- setClearColour (Linear.V4 0 0 0 (1 :: Float))
  program <- buildProgram [ GL.Setup.Vertex vertexShader, GL.Setup.Fragment fragmentShader ]
  array <- bindArray (rectVertices =<< renderingRects (pure 0 <* renderView view :: Rendering Float (Size Float)) :: [Linear.V4 Float])
  setupIO (f (program, array))
  where vertexShader = lambda "position" $ \ p ->
          set position (uniform "time" * v4 0.3 0.3 0.3 0.3 + get p)
        fragmentShader = set (out "colour") (uniform "time" + v4 0 0 1 (0.25 :: Float))

draw :: GLProgram -> GLArray Float -> Draw ()
draw program array = do
  clear [ ColourBuffer, DepthBuffer ]

  useProgram program

  t <- drawIO (realToFrac . snd . (properFraction :: POSIXTime -> (Integer, POSIXTime)) <$> getPOSIXTime)
  setUniform program "time" (Linear.V4 (sin (t * 2 * pi)) (cos (t * negate 2 * pi)) 0 0)

  bindVertexArray array
  drawArrays TriangleStrip 0 4

view :: View
view = UI.View.text "hello, world"
