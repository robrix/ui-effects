{-# LANGUAGE GADTs #-}
module UI.Drawing
( Shape(..)
, Colour(..)
, DrawingF(..)
, Drawing
, Rendering
, RenderingF
, text
, clip
, drawingBoundingRectAlgebra
, renderingBoundingRectAlgebra
, drawingCoalgebra
, renderingCoalgebra
, module Layout
) where

import Control.Applicative
import Control.Comonad.Cofree.Cofreer
import Control.Monad.Free.Freer
import Data.Functor.Algebraic
import Data.Functor.Classes
import Data.Functor.Sum
import Data.Maybe (fromMaybe)
import qualified Linear.V2 as Linear
import UI.Layout as Layout
import UI.Font
import UI.Geometry

data Shape a = Rectangle (Linear.V2 a) (Linear.V2 a)

data Colour a = RGBA !a !a !a !a

data DrawingF a f where
  Text :: Size (Maybe a) -> String -> DrawingF a f
  Clip :: Size a -> f -> DrawingF a f
  deriving (Foldable, Functor, Show)

type Drawing a = Freer (DrawingF a)
type Rendering a = Freer (RenderingF a)
type RenderingF a = Sum (DrawingF a) (LayoutF a)

text :: Size (Maybe a) -> String -> Drawing a ()
text maxSize = liftF . Text maxSize

clip :: Size a -> Drawing a b -> Drawing a b
clip size = wrap . Clip size


drawingBoundingRectAlgebra :: Real a => Algebra (Fitting (DrawingF a) a) (Rect a)
drawingBoundingRectAlgebra (Cofree (origin, _) runC r) = Rect origin $ case r of
  Pure size -> size
  Free runF r -> case runC . runF <$> r of
    Text maxSize s -> fromMaybe <$> maybe measureString measureStringForWidth (width maxSize) s <*> maxSize
    Clip size _ -> size

renderingBoundingRectAlgebra :: Real a => Algebra (Fitting (RenderingF a) a) (Rect a)
renderingBoundingRectAlgebra (Cofree a@(origin, _) runC r) = case runC <$> r of
  Pure size -> Rect origin size
  Free runF sum -> case sum of
    InL drawing -> drawingBoundingRectAlgebra (Cofree a id (Free runF drawing))
    InR layout -> fromMaybe (Rect (pure 0) (pure 0)) (layoutAlgebra (Just <$> Cofree a id (Free runF layout)))

drawingCoalgebra :: Coalgebra (Fitting (DrawingF a) a) (Point a, Size (Maybe a), Drawing a (Size a))
drawingCoalgebra (offset, maxSize, drawing) = Cofree (offset, maxSize) id $ case runFreer drawing of
  Pure size -> Pure size
  Free run l -> Free id $ case run <$> l of
    Text size string -> Text size string
    Clip size child -> Clip size (offset, maxSize, child)

renderingCoalgebra :: Real a => Coalgebra (Fitting (RenderingF a) a) (Point a, Size (Maybe a), Rendering a (Size a))
renderingCoalgebra (offset, maxSize, rendering) = Cofree (offset, maxSize) id $ case runFreer rendering of
  Pure size -> Pure size
  Free run l -> Free id $ case run <$> l of
    InL drawing -> InL $ case drawing of
      Text size string -> Text size string
      Clip size child -> Clip size (offset, maxSize, child)
    InR layout -> InR $ case layout of
      Inset by child -> Inset by (addSizeToPoint offset by, subtractSize maxSize (2 * by), child)
      Offset by child -> Offset by (liftA2 (+) offset by, subtractSize maxSize (pointSize by), child)
      Resizeable resize -> Resizeable ((,,) offset maxSize . resize)
      Measure child withMeasurement -> Measure (offset, maxSize, child) ((,,) offset maxSize . withMeasurement)
  where subtractSize maxSize size = liftA2 (-) <$> maxSize <*> (Just <$> size)
        addSizeToPoint point (Size w h) = liftA2 (+) point (Point w h)


-- Instances

instance Show a => Show1 (DrawingF a) where
  liftShowsPrec sp _ d drawing = case drawing of
    Text size string -> showsBinaryWith showsPrec showsPrec "Text" d size string
    Clip size f -> showsBinaryWith showsPrec sp "Clip" d size f
