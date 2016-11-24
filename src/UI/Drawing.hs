{-# LANGUAGE FlexibleInstances, GADTs #-}
module UI.Drawing
( Shape(..)
, Colour(..)
, DrawingF(..)
, Drawing
, Rendering
, RenderingF
, text
, clip
, drawingRectAlgebra
, drawingRectanglesAlgebra
, renderingRectAlgebra
, drawingCoalgebra
, renderingCoalgebra
, renderingRects
, module Layout
) where

import Control.Monad.Free.Freer
import Data.Functor.Algebraic
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.Functor.Sum
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup(..))
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


drawingRectAlgebra :: Real a => Algebra (Fitting (DrawingF a) a) (Rect a)
drawingRectAlgebra (Bidi (FittingState _ origin _) r) = Rect origin $ case r of
  Pure size -> size
  Free runF drawing -> case drawing of
    Text maxSize s -> size (runF (measureText (width maxSize) s))
    Clip size _ -> size

drawingRectanglesAlgebra :: Real a => Algebra (Fitting (DrawingF a) a) [Rect a]
drawingRectanglesAlgebra = collect drawingRectAlgebra

renderingRectAlgebra :: Real a => Algebra (Fitting (RenderingF a) a) (Rect a)
renderingRectAlgebra (Bidi a@(FittingState _ origin _) r) = case r of
  Pure size -> Rect origin size
  Free runF sum -> case sum of
    InL drawing -> drawingRectAlgebra (Bidi a (Free runF drawing))
    InR layout -> fromMaybe (Rect (pure 0) (pure 0)) (layoutAlgebra (Bidi a (Free (Just . runF) layout)))

drawingCoalgebra :: Coalgebra (Fitting (DrawingF a) a) (Fitting (DrawingF a) a (Drawing a (Size a)))
drawingCoalgebra = liftBidiCoalgebra drawingFCoalgebra

drawingFCoalgebra :: FittingState a -> (FittingState a -> x -> b) -> DrawingF a x -> FreerF (DrawingF a) (Size a) b
drawingFCoalgebra state run = Free (run state)

renderingCoalgebra :: Real a => Coalgebra (Fitting (RenderingF a) a) (Fitting (RenderingF a) a (Rendering a (Size a)))
renderingCoalgebra = liftBidiCoalgebra renderingFCoalgebra

renderingFCoalgebra :: Real a => FittingState a -> (FittingState a -> x -> b) -> RenderingF a x -> FreerF (RenderingF a) (Size a) b
renderingFCoalgebra state run renderingF = case renderingF of
  InL drawingF -> hoistFreerF InL $ drawingFCoalgebra state run drawingF
  InR layoutF -> hoistFreerF InR $ layoutFCoalgebra state run layoutF

renderingRects :: Real a => Rendering a (Size a) -> [Rect a]
renderingRects = hylo (collect renderingRectAlgebra) renderingCoalgebra . Bidi (FittingState Full (pure 0) (pure Nothing)) . runFreer


-- Instances

instance Real a => Semigroup (Rendering a (Size a)) where
  (<>) top bottom = do
    Size w1 h1 <- top
    Size w2 h2 <- wrapR $ Offset (Point 0 h1) bottom
    pure $ Size (max w1 w2) (h1 + h2)

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

instance Eq2 DrawingF where
  liftEq2 eqA eqF d1 d2 = case (d1, d2) of
    (Text m1 s1, Text m2 s2) -> liftEq (liftEq eqA) m1 m2 && s1 == s2
    (Clip s1 c1, Clip s2 c2) -> liftEq eqA s1 s2 && eqF c1 c2
    _ -> False

instance Eq a => Eq1 (DrawingF a) where
  liftEq = liftEq2 (==)

instance (Eq a, Eq f) => Eq (DrawingF a f) where
  (==) = liftEq (==)
