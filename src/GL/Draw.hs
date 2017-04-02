{-# LANGUAGE FlexibleInstances, GADTs, RankNTypes #-}
module GL.Draw where

import Control.Monad.Free.Freer
import Control.Monad.IO.Class
import Data.Bits
import GL.Array
import GL.Exception
import GL.Geometry
import GL.Program
import GL.Shader
import Graphics.GL.Core41

data Buffer = ColourBuffer | DepthBuffer | StencilBuffer

data DrawF a where
  Clear :: [Buffer] -> DrawF ()
  UseProgram :: GLProgram -> DrawF ()
  SetUniform :: GLProgramUniform v => GLProgram -> Var (Shader v) -> v -> DrawF ()
  DrawGeometry :: GeometryArray n -> DrawF ()
  RunIO :: IO a -> DrawF a

type Draw = Freer DrawF


clear :: [Buffer] -> Draw ()
clear = liftF . Clear

useProgram :: GLProgram -> Draw ()
useProgram = liftF . UseProgram

setUniform :: GLProgramUniform v => GLProgram -> Var (Shader v) -> v -> Draw ()
setUniform program var value = liftF (SetUniform program var value)

drawGeometry :: GeometryArray n -> Draw ()
drawGeometry = liftF . DrawGeometry


runDraw :: Draw a -> IO a
runDraw = iterFreerA $ \ d rest -> case d of
  Clear buffers -> do
    glClear $ foldr (.|.) 0 ((\ b -> case b of
      ColourBuffer -> GL_COLOR_BUFFER_BIT
      DepthBuffer -> GL_DEPTH_BUFFER_BIT
      StencilBuffer -> GL_STENCIL_BUFFER_BIT) <$> buffers)
    checkingGLError (rest ())
  UseProgram program -> do
    glUseProgram (unGLProgram program)
    checkingGLError (rest ())
  SetUniform program var value -> do
    setUniformValue program var value
    checkingGLError (rest ())
  DrawGeometry (GeometryArray ranges array) -> do
    glBindVertexArray (unGLArray array)
    _ <- traverse drawRange ranges
    checkingGLError (rest ())
  RunIO io -> io >>= rest
  where drawRange (ArrayRange mode from count) = checkingGLError $ glDrawArrays (case mode of
          Points -> GL_POINTS
          Lines -> GL_LINES
          LineLoop -> GL_LINE_LOOP
          LineStrip -> GL_LINE_STRIP
          Triangles -> GL_TRIANGLES
          TriangleStrip -> GL_TRIANGLE_STRIP) (fromIntegral from) (fromIntegral (from + count))


-- Instances

instance MonadIO Draw where
  liftIO = liftF . RunIO
