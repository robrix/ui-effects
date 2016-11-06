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

import Control.Action
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

type Drawing a = Freer (Action (DrawingF a))
type Rendering a = Freer (RenderingF a)
type RenderingF a = Sum (Action (DrawingF a)) (LayoutF a)

text :: Size (Maybe a) -> String -> Drawing a (Size a)
text maxSize = liftF . liftAction . Text maxSize

clip :: Size a -> Drawing a b -> Drawing a b
clip size = wrap . liftAction . Clip size


drawingBoundingRectAlgebra :: Real a => Algebra (Fitting (Action (DrawingF a)) a) (Rect a)
drawingBoundingRectAlgebra (Cofree (origin, _) runC r) = Rect origin $ case r of
  Pure size -> size
  Free runF action | Action drawing runA <- runC . runF <$> action -> case drawing of
    Text maxSize s -> size (runA (measureText (width maxSize) s))
    Clip size _ -> size

drawingRectanglesAlgebra :: Real a => Algebra (Fitting (Action (DrawingF a)) a) [Rect a]
drawingRectanglesAlgebra = collect drawingBoundingRectAlgebra

renderingBoundingRectAlgebra :: Real a => Algebra (Fitting (RenderingF a) a) (Rect a)
renderingBoundingRectAlgebra (Cofree a@(origin, _) runC r) = case runC <$> r of
  Pure size -> Rect origin size
  Free runF sum -> case sum of
    InL drawing -> drawingBoundingRectAlgebra (Cofree a id (Free runF drawing))
    InR layout -> fromMaybe (Rect (pure 0) (pure 0)) (layoutAlgebra (Just <$> Cofree a id (Free runF layout)))

drawingCoalgebra :: Real a => Coalgebra (Fitting (Action (DrawingF a)) a) (Point a, Size (Maybe a), Drawing a (Size a))
drawingCoalgebra (offset, maxSize, drawing) = Cofree (offset, maxSize) id $ case runFreer drawing of
  Pure size -> Pure size
  Free runF action | Action drawing runA <- runF <$> action -> Free id $ case drawing of
    Text size string -> Action (Text size string) (const (offset, maxSize, runA (measureText (width size) string)))
    Clip size child -> Action (Clip size (offset, maxSize, runA child)) id

renderingCoalgebra :: Real a => Coalgebra (Fitting (RenderingF a) a) (Point a, Size (Maybe a), Rendering a (Size a))
renderingCoalgebra (offset, maxSize, rendering) = Cofree (offset, maxSize) id $ case runFreer rendering of
  Pure size -> Pure size
  Free run l -> Free id $ case run <$> l of
    InL (Action drawing runA) -> InL $ case drawing of
      Text size string -> Action (Text size string) (const (offset, maxSize, runA (measureText (width size) string)))
      Clip size child -> Action (Clip size (offset, maxSize, runA child)) id
    InR layout -> InR $ case layout of
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
