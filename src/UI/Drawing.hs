{-# LANGUAGE GADTs #-}
module UI.Drawing
( Shape(..)
, Colour(..)
, DrawingF(..)
, Drawing
, Rendering
, setStroke
, setFill
, stroke
, fill
, text
, clip
, module Layout
) where

import Control.Monad.Free.Freer
import Data.Functor.Sum
import qualified Linear.V2 as Linear
import UI.Layout as Layout
import UI.Geometry

data Shape a = Rectangle (Linear.V2 a) (Linear.V2 a)

data Colour a = RGBA !a !a !a !a

data DrawingF a f where
  SetStroke :: Colour a -> DrawingF a f
  SetFill :: Colour a -> DrawingF a f
  Stroke :: Shape a -> DrawingF a f
  Fill :: Shape a -> DrawingF a f
  Text :: Size (Maybe a) -> String -> DrawingF a f
  Clip :: Size a -> f -> DrawingF a f
  deriving Functor

type Drawing a = Freer (DrawingF a)
type Rendering a = Freer (Sum (DrawingF a) (LayoutF a))

setStroke :: Colour a -> Drawing a ()
setStroke c = liftF $ SetStroke c

setFill :: Colour a -> Drawing a ()
setFill c = liftF $ SetFill c

stroke :: Shape a -> Drawing a ()
stroke s = liftF $ Stroke s

fill :: Shape a -> Drawing a ()
fill s = liftF $ Fill s

text :: Size (Maybe a) -> String -> Drawing a ()
text maxSize = liftF . Text maxSize

clip :: Size a -> Drawing a b -> Drawing a b
clip size = wrap . Clip size
