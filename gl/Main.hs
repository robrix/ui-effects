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
import qualified Linear.Matrix as Linear
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

rectVertices :: Num a => Rect a -> [Linear.V4 a]
rectVertices (Rect (Point x y) (Size w h)) =
  [ Linear.V4 x        y      0 1
  , Linear.V4 x       (y + h) 0 1
  , Linear.V4 (x + w)  y      0 1
  , Linear.V4 (x + w) (y + h) 0 1
  ]

setup :: ((GLProgram, GLArray Float) -> IO a) -> Setup a
setup f = do
  enable DepthTest
  enable Blending
  setDepthFunc Less
  setBlendFactors SourceAlpha OneMinusSourceAlpha
  setClearColour (Linear.V4 0 0 0 (1 :: Float))
  program <- buildProgram [ GL.Setup.Vertex vertexShader, GL.Setup.Fragment fragmentShader ]
  array <- bindArray (rectVertices =<< renderingRects (pure 0 <* renderView view :: Rendering Float (Size Float)) :: [Linear.V4 Float])
  setupIO (f (program, array))
  where vertexShader = lambda "position" $ \ p ->
          set position ((get p + v4 (negate 1) (negate 1) 0 0) * v4 (1/1024) (1/768) 1 1)
        fragmentShader = set (out "colour") (uniform "time" + v4 0 0 1 (0.5 :: Float))

orthographic :: Fractional a => a -> a -> a -> a -> a -> a -> Linear.M44 a
orthographic left right bottom top near far = Linear.V4
  (Linear.V4 (2 / (right - left))  0                    0                         tx)
  (Linear.V4  0                   (2 / (top - bottom))  0                         ty)
  (Linear.V4  0                    0                   (negate 2 / (far - near))  tz)
  (Linear.V4  0                    0                    0                         1)
  where tx = negate ((right + left) / (right - left))
        ty = negate ((top + bottom) / (top - bottom))
        tz = negate ((far + near) / (far - near))

draw :: GLProgram -> GLArray Float -> Draw ()
draw program array = do
  clear [ ColourBuffer, DepthBuffer ]

  useProgram program

  t <- drawIO (realToFrac . snd . (properFraction :: POSIXTime -> (Integer, POSIXTime)) <$> getPOSIXTime)
  setUniform program "time" (Linear.V4 (sin (t * 2 * pi)) (cos (t * negate 2 * pi)) 0 0 :: Linear.V4 Float)

  bindVertexArray array
  drawArrays TriangleStrip 0 4

view :: View
view = UI.View.text "hello, world"
