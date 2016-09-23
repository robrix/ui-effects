module UI.Drawing where

import Control.Action
import Control.Applicative.Free.Freer
import Linear.V2

data Shape a
  = Rectangle (V2 a) (V2 a)
  | Circle (V2 a) (V2 a)

data Colour a = RGBA !a !a !a !a

data Gradient a
  = Linear (V2 a) (Colour a) (V2 a) (Colour a)

data Material a
  = Colour (Colour a)
  | Gradient (Gradient a)

data DrawingF a
  = SetStroke (Colour a)
  | SetFill (Colour a)
  | Stroke (Shape a)
  | Fill (Shape a)

type Drawing a = Freer (Action DrawingF) a

setStroke :: Colour a -> Drawing a
setStroke c = liftF . liftAction $ SetStroke c

setFill :: Colour a -> Drawing a
setFill c = liftF . liftAction $ SetFill c

stroke :: Shape a -> Drawing a
stroke s = liftF . liftAction $ Stroke s

fill :: Shape a -> Drawing a
fill s = liftF . liftAction $ Fill s
