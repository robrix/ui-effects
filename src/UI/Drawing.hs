module UI.Drawing where

import Control.Monad.Free.Freer
import Linear.V2
import Linear.V4

data Shape a
  = Rectangle (V2 a) (V2 a)
  | Circle (V2 a) (V2 a)

data Colour a = RGBA !a !a !a !a

data Gradient a
  = Linear (V2 a) (Colour a) (V2 a) (Colour a)

data Material a
  = Colour (Colour a)
  | Gradient (Gradient a)

data FragmentF f
  = Coord (V4 Float -> f)
  | SampleID (Int -> f)
  | NumSamples (Int -> f)
  | PointCoord (V2 Float -> f)
  | SamplePosition (V2 Float -> f)
  | SetFragDepth Float f
  | SetFragColour (Colour Float) f

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
