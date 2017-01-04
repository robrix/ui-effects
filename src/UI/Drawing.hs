{-# LANGUAGE FlexibleInstances, GADTs #-}
module UI.Drawing
( DrawingF(..)
, Drawing
, Rendering
, RenderingF
, text
, clip
, background
, rgba
, drawingRectAlgebra
, drawingBackgroundRectAlgebra
, renderingRectAlgebra
, renderingBackgroundRectAlgebra
, drawingCoalgebra
, renderingCoalgebra
, renderingRects
, renderingBackgroundRects
, renderingBackgroundRects'
, module Layout
) where

import Control.Monad.Free.Freer
import Data.Bifunctor
import Data.Either (rights)
import Data.Functor.Algebraic
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.Functor.Sum
import Data.Maybe (catMaybes)
import Data.Semigroup (Semigroup(..))
import qualified Linear.V4 as Linear
import UI.Layout as Layout
import UI.Font
import UI.Geometry

data DrawingF a f where
  Text :: Size (Maybe a) -> String -> DrawingF a (Size a)
  Clip :: Size a -> f -> DrawingF a f
  Background :: Linear.V4 a -> f -> DrawingF a f

type Drawing a = Freer (DrawingF a)
type Rendering a = Freer (RenderingF a)
type RenderingF a = Sum (DrawingF a) (LayoutF a)

text :: Size (Maybe a) -> String -> Drawing a (Size a)
text = (liftF .) . Text

clip :: Size a -> Drawing a b -> Drawing a b
clip = (wrap .) . Clip

background :: Linear.V4 a -> Drawing a b -> Drawing a b
background = (wrap .) . Background

rgba :: a -> a -> a -> a -> Linear.V4 a
rgba = Linear.V4

drawingRectAlgebra :: Real a => Algebra (Fitting (DrawingF a) a) (Maybe (Rect a))
drawingRectAlgebra (Bidi (FittingState _ origin _) r) = case r of
  Pure size -> Just (Rect origin size)
  Free runF drawing -> case drawing of
    Text maxSize s -> Rect origin . size <$> runF (measureText (width maxSize) s)
    Clip size _ -> Just (Rect origin size)
    Background _ child -> runF child

drawingBackgroundRectAlgebra :: Real a => Algebra (Fitting (DrawingF a) a) (Either (Rect a) (Rect a))
drawingBackgroundRectAlgebra (Bidi (FittingState _ origin _) r) = case r of
  Pure size -> Left (Rect origin size)
  Free runF drawing -> case drawing of
    Text maxSize s -> Left (Rect origin (either size size (runF (measureText (width maxSize) s))))
    Clip maxSize child -> bimap (clipRect maxSize) (clipRect maxSize) (runF child)
    Background _ child -> Right (Rect origin (either size size (runF child)))
  where clip toSize size = min <$> size <*> toSize
        clipRect toSize = Rect origin . clip toSize . size

renderingRectAlgebra :: Real a => Algebra (Fitting (RenderingF a) a) (Maybe (Rect a))
renderingRectAlgebra (Bidi a@(FittingState _ origin _) r) = case r of
  Pure size -> Just (Rect origin size)
  Free runF sum -> sumAlgebra drawingRectAlgebra layoutAlgebra $ hoistSum (Bidi a . Free runF) (Bidi a . Free runF) sum

renderingBackgroundRectAlgebra :: Real a => Algebra (Fitting (RenderingF a) a) (Either (Rect a) (Rect a))
renderingBackgroundRectAlgebra (Bidi a@(FittingState _ origin _) r) = case r of
  Pure size -> Left (Rect origin size)
  Free runF sum -> sumAlgebra drawingBackgroundRectAlgebra layoutBackgroundRectAlgebra $ hoistSum (Bidi a . Free runF) (Bidi a . Free runF) sum

drawingCoalgebra :: Coalgebra (Fitting (DrawingF a) a) (Fitting (DrawingF a) a (Drawing a (Size a)))
drawingCoalgebra = liftBidiCoalgebra drawingFCoalgebra

drawingFCoalgebra :: CoalgebraFragment (DrawingF a) (FittingState a) (Size a)
drawingFCoalgebra state run = Free (run state)

renderingCoalgebra :: Real a => Coalgebra (Fitting (RenderingF a) a) (Fitting (RenderingF a) a (Rendering a (Size a)))
renderingCoalgebra = liftBidiCoalgebra (liftSumCoalgebra drawingFCoalgebra layoutFCoalgebra)

renderingRects :: Real a => Rendering a (Size a) -> [Rect a]
renderingRects = hylo (wrapAlgebra catMaybes (fmap Just) (collect renderingRectAlgebra)) renderingCoalgebra . Bidi (FittingState Full (pure 0) (pure Nothing)) . runFreer

renderingBackgroundRects :: Real a => Rendering a (Size a) -> [Rect a]
renderingBackgroundRects = rights . renderingBackgroundRects'

renderingBackgroundRects' :: Real a => Rendering a (Size a) -> [Either (Rect a) (Rect a)]
renderingBackgroundRects' = hylo (collect renderingBackgroundRectAlgebra) renderingCoalgebra . Bidi (FittingState Full (pure 0) (pure Nothing)) . runFreer


-- Instances

instance Real a => Semigroup (Rendering a (Size a)) where
  (<>) top bottom = do
    Size w1 h1 <- top
    Size w2 h2 <- wrapR $ Offset (Point 0 h1) bottom
    pure $ Size (max w1 w2) h2

instance Show a => Show1 (DrawingF a) where
  liftShowsPrec sp _ d drawing = case drawing of
    Text size string -> showsBinaryWith showsPrec showsPrec "Text" d size string
    Clip size f -> showsBinaryWith showsPrec sp "Clip" d size f
    Background colour f -> showsBinaryWith showsPrec sp "Fill" d colour f

instance (Show a, Show b) => Show (DrawingF a b) where
  showsPrec = liftShowsPrec showsPrec showList

instance Real a => Foldable (DrawingF a) where
  foldMap f drawing = case drawing of
    Text (Size w _) s -> f (measureText w s)
    Clip _ child -> f child
    Background _ child -> f child

instance Eq2 DrawingF where
  liftEq2 eqA eqF d1 d2 = case (d1, d2) of
    (Text m1 s1, Text m2 s2) -> liftEq (liftEq eqA) m1 m2 && s1 == s2
    (Clip s1 c1, Clip s2 c2) -> liftEq eqA s1 s2 && eqF c1 c2
    (Background v1 c1, Background v2 c2) -> liftEq eqA v1 v2 && eqF c1 c2
    _ -> False

instance Eq a => Eq1 (DrawingF a) where
  liftEq = liftEq2 (==)

instance (Eq a, Eq f) => Eq (DrawingF a f) where
  (==) = liftEq (==)
