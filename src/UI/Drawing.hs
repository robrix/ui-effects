module UI.Drawing where

import Control.Monad.Free.Freer

data Vector2 a = Vector2 !a !a
data Vector4 a = Vector4 !a !a !a !a

data Shape a
  = Rectangle (Vector2 a) (Vector2 a)
  | Circle (Vector2 a) (Vector2 a)

data Colour a = RGBA !a !a !a !a

data Gradient a
  = Linear (Vector2 a) (Colour a) (Vector2 a) (Colour a)

data Material a
  = Colour (Colour a)
  | Gradient (Gradient a)

data FragmentF f
  = Coord (Vector4 Float -> f)
  | SampleID (Int -> f)
  | NumSamples (Int -> f)
  | PointCoord (Vector2 Float -> f)
  | SetFragDepth Float f

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
