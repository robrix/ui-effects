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
, drawingRectanglesAlgebra
, renderingBoundingRectAlgebra
, drawingCoalgebra
, renderingCoalgebra
, renderingRects
, module Layout
) where

import Control.Applicative
import Control.Comonad.Cofree.Cofreer
import Control.Monad.Free.Freer
import Data.Functor.Algebraic
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.Functor.Sum
import Data.Maybe (fromMaybe)
import qualified Linear.V2 as Linear
import UI.Layout as Layout
import UI.Font
import UI.Geometry

data Shape a = Rectangle (Linear.V2 a) (Linear.V2 a)

data Colour a = RGBA !a !a !a !a

data DrawingF a f where
  Text :: Size (Maybe a) -> String -> DrawingF a (Size a)
  Clip :: Size a -> f -> DrawingF a f

type Drawing a = Freer (DrawingF a)
type Rendering a = Freer (RenderingF a)
type RenderingF a = Sum (DrawingF a) (LayoutF a)

text :: Size (Maybe a) -> String -> Drawing a (Size a)
text maxSize = Freer . Free pure . Text maxSize

clip :: Size a -> Drawing a b -> Drawing a b
clip size = wrap . Clip size


drawingBoundingRectAlgebra :: Real a => Algebra (Fitting (DrawingF a) a) (Rect a)
drawingBoundingRectAlgebra (Cofree (origin, _) runC r) = Rect origin $ case r of
  Pure size -> size
  Free runF drawing -> case drawing of
    Text maxSize s -> size (runC (runF (measureText (width maxSize) s)))
    Clip size _ -> size

drawingRectanglesAlgebra :: Real a => Algebra (Fitting (DrawingF a) a) [Rect a]
drawingRectanglesAlgebra = collect drawingBoundingRectAlgebra

renderingBoundingRectAlgebra :: Real a => Algebra (Fitting (RenderingF a) a) (Rect a)
renderingBoundingRectAlgebra (Cofree a@(origin, _) runC r) = case runC <$> r of
  Pure size -> Rect origin size
  Free runF sum -> case sum of
    InL drawing -> drawingBoundingRectAlgebra (Cofree a id (Free runF drawing))
    InR layout -> fromMaybe (Rect (pure 0) (pure 0)) (layoutAlgebra (Just <$> Cofree a id (Free runF layout)))

drawingCoalgebra :: Coalgebra (Fitting (DrawingF a) a) (Point a, Size (Maybe a), Drawing a (Size a))
drawingCoalgebra (offset, maxSize, drawing) = Cofree (offset, maxSize) id $ case runFreer drawing of
  Pure size -> Pure size
  Free runF drawing -> case drawing of
    Text size string -> Free ((,,) offset maxSize . pure) (Text size string)
    Clip size child -> Free id (Clip size (offset, maxSize, runF child))

renderingCoalgebra :: Real a => Coalgebra (Fitting (RenderingF a) a) (Point a, Size (Maybe a), Rendering a (Size a))
renderingCoalgebra (offset, maxSize, rendering) = Cofree (offset, maxSize) id $ case runFreer rendering of
  Pure size -> Pure size
  Free runF rendering -> case rendering of
    InL drawing -> case drawing of
      Text size string -> Free ((,,) offset maxSize . pure) $ InL (Text size string)
      Clip size child -> Free id (InL (Clip size (offset, maxSize, runF child)))
    InR layout -> Free id . InR $ case runF <$> layout of
      Inset by child -> Inset by (addSizeToPoint offset by, subtractSize maxSize (2 * by), child)
      Offset by child -> Offset by (liftA2 (+) offset by, subtractSize maxSize (pointSize by), child)
      Resizeable resize -> Resizeable ((,,) offset maxSize . resize)
  where subtractSize maxSize size = liftA2 (-) <$> maxSize <*> (Just <$> size)
        addSizeToPoint point (Size w h) = liftA2 (+) point (Point w h)

renderingRects :: Real a => Rendering a (Size a) -> [Rect a]
renderingRects = hylo (collect renderingBoundingRectAlgebra) renderingCoalgebra . (,,) (pure 0) (pure Nothing)


-- Instances

instance Show a => Show1 (DrawingF a) where
  liftShowsPrec sp _ d drawing = case drawing of
    Text size string -> showsBinaryWith showsPrec showsPrec "Text" d size string
    Clip size f -> showsBinaryWith showsPrec sp "Clip" d size f

instance (Show a, Show b) => Show (DrawingF a b) where
  showsPrec = liftShowsPrec showsPrec showList

instance Real a => Foldable (DrawingF a) where
  foldMap f drawing = case drawing of
    Text (Size w _) s -> f (measureText w s)
    Clip _ child -> f child
