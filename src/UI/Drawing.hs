module UI.Drawing where

import Control.Monad.Free.Freer

data Vector2 a = Vector2 !a !a

data Shape a
  = Rectangle (Vector2 a) (Vector2 a)
  | Circle (Vector2 a) (Vector2 a)

data Colour a = RGBA !a !a !a !a

data Gradient a
  = Linear (Vector2 a) (Colour a) (Vector2 a) (Colour a)

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
