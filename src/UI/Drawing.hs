{-# LANGUAGE GADTs #-}
module UI.Drawing where

import Control.Action
import Control.Applicative.Free.Freer
import Linear.V2

data Shape a
  = Rectangle (V2 a) (V2 a)
  | Circle a (V2 a)

data Colour a = RGBA !a !a !a !a

data Gradient a
  = Linear (V2 a) (Colour a) (V2 a) (Colour a)

data Material a
  = Colour (Colour a)
  | Gradient (Gradient a)

data DrawingF a where
  SetStroke :: Colour a -> DrawingF ()
  SetFill :: Colour a -> DrawingF ()
  Stroke :: Shape a -> DrawingF ()
  Fill :: Shape a -> DrawingF ()

type Drawing a = Freer (Action DrawingF) a

setStroke :: Colour a -> Drawing ()
setStroke c = liftF . liftAction $ SetStroke c

setFill :: Colour a -> Drawing ()
setFill c = liftF . liftAction $ SetFill c

stroke :: Shape a -> Drawing ()
stroke s = liftF . liftAction $ Stroke s

fill :: Shape a -> Drawing ()
fill s = liftF . liftAction $ Fill s
