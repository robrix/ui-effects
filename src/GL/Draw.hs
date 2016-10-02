{-# LANGUAGE GADTs #-}
module GL.Draw where

import Control.Action
import Control.Monad.Free.Freer
import Data.Bits
import GL.Array
import GL.Program
import Graphics.GL.Core41
import Linear.V4 as Linear

data Buffer = Colour | Depth | Stencil
data Mode = Lines | LineLoop | LineStrip | Triangles | TriangleStrip

data DrawF a where
  Clear :: [Buffer] -> DrawF ()
  UseProgram :: GLProgram -> DrawF ()
  SetUniform :: GLProgram -> String -> Linear.V4 Float -> DrawF ()
  BindVertexArray :: GLArray n -> DrawF ()
  DrawArrays :: Mode -> Int -> Int -> DrawF ()

type Draw = Freer (Action DrawF)


clear :: [Buffer] -> Draw ()
clear = liftF . liftAction . Clear

useProgram :: GLProgram -> Draw ()
useProgram = liftF . liftAction . UseProgram

setUniform :: GLProgram -> String -> Linear.V4 Float -> Draw ()
setUniform program var value = liftF (liftAction (SetUniform program var value))

bindVertexArray :: GLArray n -> Draw ()
bindVertexArray = liftF . liftAction . BindVertexArray

drawArrays :: Mode -> Int -> Int -> Draw ()
drawArrays mode from to = liftF (liftAction (DrawArrays mode from to))


runDraw :: Draw a -> IO a
runDraw = iterM $ \ d -> case d of
  Action (Clear buffers) rest -> do
    glClear $ foldr (.|.) 0 ((\ b -> case b of
      Colour -> GL_COLOR_BUFFER_BIT
      Depth -> GL_DEPTH_BUFFER_BIT
      Stencil -> GL_STENCIL_BUFFER_BIT) <$> buffers)
    rest ()
  Action (UseProgram program) rest -> do
    glUseProgram (unGLProgram program)
    rest ()
  Action (SetUniform program var value) rest -> do
    setUniformValue program var value
    rest ()
  Action (BindVertexArray array) rest -> do
    glBindVertexArray (unGLArray array)
    rest ()
  Action (DrawArrays mode from to) rest -> do
    glDrawArrays (case mode of
      Lines -> GL_LINES
      LineLoop -> GL_LINE_LOOP
      LineStrip -> GL_LINE_STRIP
      Triangles -> GL_TRIANGLES
      TriangleStrip -> GL_TRIANGLE_STRIP) (fromIntegral from) (fromIntegral to)
    rest ()
