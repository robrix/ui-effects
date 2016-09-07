module UI.Drawing where

import Control.Monad.Free.Freer

data Vector a = Vector !a !a

data Shape a
  = Rectangle (Vector a) (Vector a)
  | Circle (Vector a) (Vector a)

data Colour a = RGBA !a !a !a !a

data Gradient a
  = Linear (Vector a) (Colour a) (Vector a) (Colour a)

data Material a
  = Colour (Colour a)
  | Gradient (Gradient a)

data DrawingF a f
  = SetStroke (Colour a) f
  | SetFill (Colour a) f
  | Stroke (Shape a) f
  | Fill (Shape a) f
  deriving Functor

type Drawing a = Freer (DrawingF a)

setStroke :: Colour a -> Drawing a ()
setStroke c = liftF $ SetStroke c ()

setFill :: Colour a -> Drawing a ()
setFill c = liftF $ SetFill c ()

stroke :: Shape a -> Drawing a ()
stroke s = liftF $ Stroke s ()

fill :: Shape a -> Drawing a ()
fill s = liftF $ Fill s ()
