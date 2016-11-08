{-# LANGUAGE GADTs, RankNTypes #-}
module GL.Draw where

import Control.Monad.Free.Freer
import Data.Bits
import GL.Array
import GL.Exception
import GL.Program
import Graphics.GL.Core41
import Prelude hiding (IO)

data Buffer = ColourBuffer | DepthBuffer | StencilBuffer
data Mode = Points | Lines | LineLoop | LineStrip | Triangles | TriangleStrip

data DrawF a where
  Clear :: [Buffer] -> DrawF ()
  UseProgram :: GLProgram -> DrawF ()
  SetUniform :: GLProgramUniform v => GLProgram -> String -> v -> DrawF ()
  BindVertexArray :: GLArray n -> DrawF ()
  DrawArrays :: Mode -> Int -> Int -> DrawF ()
  RunIO :: IO a -> DrawF a

type Draw = Freer DrawF


clear :: [Buffer] -> Draw ()
clear = liftF . Clear

useProgram :: GLProgram -> Draw ()
useProgram = liftF . UseProgram

setUniform :: GLProgramUniform v => GLProgram -> String -> v -> Draw ()
setUniform program var value = liftF (SetUniform program var value)

bindVertexArray :: GLArray n -> Draw ()
bindVertexArray = liftF . BindVertexArray

drawArrays :: Mode -> Int -> Int -> Draw ()
drawArrays mode from to = liftF (DrawArrays mode from to)

drawIO :: IO a -> Draw a
drawIO = liftF . RunIO


runDraw :: Draw a -> IO a
runDraw = iterFreerA $ \ rest d -> case d of
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
  BindVertexArray array -> do
    glBindVertexArray (unGLArray array)
    checkingGLError (rest ())
  DrawArrays mode from to -> do
    glDrawArrays (case mode of
      Points -> GL_POINTS
      Lines -> GL_LINES
      LineLoop -> GL_LINE_LOOP
      LineStrip -> GL_LINE_STRIP
      Triangles -> GL_TRIANGLES
      TriangleStrip -> GL_TRIANGLE_STRIP) (fromIntegral from) (fromIntegral to)
    checkingGLError (rest ())
  RunIO io -> io >>= rest
