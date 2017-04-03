{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, RankNTypes, TypeOperators #-}
module GL.Draw where

import Control.Monad.Free.Freer
import Data.Bits
import Data.Functor.Union
import GL.Array
import GL.Exception
import GL.Geometry
import GL.Program
import GL.Shader
import Graphics.GL.Core41

data Buffer = ColourBuffer | DepthBuffer | StencilBuffer

data Draw a where
  Clear :: [Buffer] -> Draw ()
  UseProgram :: GLProgram -> Draw ()
  SetUniform :: GLProgramUniform v => GLProgram -> Var (Shader v) -> v -> Draw ()
  DrawGeometry :: GeometryArray n -> Draw ()


clear :: InUnion fs Draw => [Buffer] -> Eff fs ()
clear = send . Clear

useProgram :: InUnion fs Draw => GLProgram -> Eff fs ()
useProgram = send . UseProgram

setUniform :: InUnion fs Draw => GLProgramUniform v => GLProgram -> Var (Shader v) -> v -> Eff fs ()
setUniform program var value = send (SetUniform program var value)

drawGeometry :: InUnion fs Draw => GeometryArray n -> Eff fs ()
drawGeometry = send . DrawGeometry


runDraw :: InUnion fs IO => Eff (Draw ': fs) a -> Eff fs a
runDraw = iterFreerA $ \ union yield -> case union of
  Here d -> case d of
    Clear buffers -> do
      glClear $ foldr (.|.) 0 ((\ b -> case b of
        ColourBuffer -> GL_COLOR_BUFFER_BIT
        DepthBuffer -> GL_DEPTH_BUFFER_BIT
        StencilBuffer -> GL_STENCIL_BUFFER_BIT) <$> buffers)
      checkingGLError (yield ())
    UseProgram program -> do
      glUseProgram (unGLProgram program)
      checkingGLError (yield ())
    SetUniform program var value -> do
      setUniformValue program var value
      checkingGLError (yield ())
    DrawGeometry (GeometryArray ranges array) -> do
      glBindVertexArray (unGLArray array)
      _ <- traverse drawRange ranges
      checkingGLError (yield ())
  There t -> t `Then` yield
  where drawRange (ArrayRange mode from count) = checkingGLError $ glDrawArrays (case mode of
          Points -> GL_POINTS
          Lines -> GL_LINES
          LineLoop -> GL_LINE_LOOP
          LineStrip -> GL_LINE_STRIP
          Triangles -> GL_TRIANGLES
          TriangleStrip -> GL_TRIANGLE_STRIP) (fromIntegral from) (fromIntegral (from + count))
