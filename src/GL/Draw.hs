{-# LANGUAGE GADTs #-}
module GL.Draw where

import Control.Action
import Control.Monad.Free.Freer
import GL.Array
import GL.Program
import Linear.V4 as Linear

data Buffer = Colour | Depth | Stencil
data Mode = Lines | LineLoop | LineStrip | Triangles | TriangleStrip

data DrawF a where
  Clear :: [Buffer] -> DrawF ()
  UseProgram :: GLProgram -> DrawF ()
  SetUniform :: GLProgram -> String -> Linear.V4 a -> DrawF ()
  BindVertexArray :: GLArray n -> DrawF ()
  DrawArrays :: Mode -> Int -> Int -> DrawF ()

type Draw = Freer (Action DrawF)


clear :: [Buffer] -> Draw ()
clear = liftF . liftAction . Clear

useProgram :: GLProgram -> Draw ()
useProgram = liftF . liftAction . UseProgram

setUniform :: GLProgram -> String -> Linear.V4 a -> Draw ()
setUniform program var value = liftF (liftAction (SetUniform program var value))

bindVertexArray :: GLArray n -> Draw ()
bindVertexArray = liftF . liftAction . BindVertexArray
