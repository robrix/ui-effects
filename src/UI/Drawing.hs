{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs #-}
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
, renderingRectAlgebra
, drawingCoalgebra
, renderingCoalgebra
, renderingRects
, module Layout
) where

import Control.Monad.Free.Freer as Freer
import Control.Monad.Trans.Free.Freer as FreerF
import Data.Functor.Algebraic
import Data.Functor.Classes
import Data.Functor.Foldable hiding (Nil)
import Data.Functor.Union
import Data.Maybe (catMaybes)
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
type RenderingF a = Union '[DrawingF a, LayoutF a]

text :: InUnion fs (DrawingF a) => Size (Maybe a) -> String -> Freer (Union fs) (Size a)
text maxSize str = inj (Text maxSize str) `Freer.Then` return

clip :: InUnion fs (DrawingF a) => Size a -> Freer (Union fs) b -> Freer (Union fs) b
clip size drawing = wrapU (Clip size drawing)


drawingRectAlgebra :: Real a => Algebra (Fitting (DrawingF a) a) (Maybe (Rect a))
drawingRectAlgebra (Bidi (FittingState _ origin _) r) = Rect origin <$> case r of
  FreerF.Return size -> Just size
  drawing `FreerF.Then` runF -> case drawing of
    Text maxSize s -> size <$> runF (measureText (width maxSize) s)
    Clip size _ -> Just size

renderingRectAlgebra :: Real a => Algebra (Fitting (RenderingF a) a) (Maybe (Rect a))
renderingRectAlgebra (Bidi a@(FittingState _ origin _) r) = case r of
  FreerF.Return size -> Just (Rect origin size)
  union `FreerF.Then` continue -> caseU union
    $  (\ d -> drawingRectAlgebra (Bidi a (d `FreerF.Then` continue)))
    :. (\ l -> layoutAlgebra (Bidi a (l `FreerF.Then` continue)))
    :. Nil

drawingCoalgebra :: Coalgebra (Fitting (DrawingF a) a) (Fitting (DrawingF a) a (Drawing a (Size a)))
drawingCoalgebra = liftBidiCoalgebra drawingFCoalgebra

drawingFCoalgebra :: CoalgebraFragment (DrawingF a) (FittingState a) (Size a)
drawingFCoalgebra state run = flip FreerF.Then (run state)

renderingCoalgebra :: Real a => Coalgebra (Fitting (RenderingF a) a) (Fitting (RenderingF a) a (Rendering a (Size a)))
renderingCoalgebra = liftBidiCoalgebra (\ state run union -> caseU union
  $  (\ d -> hoistFreerF inj (drawingFCoalgebra state run d))
  :. (\ l -> hoistFreerF inj (layoutFCoalgebra state run l))
  :. Nil)

renderingRects :: Real a => Rendering a (Size a) -> [Rect a]
renderingRects = hylo (wrapAlgebra catMaybes (fmap Just) (collect renderingRectAlgebra)) renderingCoalgebra . Bidi (FittingState Full (pure 0) (pure Nothing)) . project


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

instance Eq2 DrawingF where
  liftEq2 eqA eqF d1 d2 = case (d1, d2) of
    (Text m1 s1, Text m2 s2) -> liftEq (liftEq eqA) m1 m2 && s1 == s2
    (Clip s1 c1, Clip s2 c2) -> liftEq eqA s1 s2 && eqF c1 c2
    _ -> False

instance Eq a => Eq1 (DrawingF a) where
  liftEq = liftEq2 (==)

instance (Eq a, Eq f) => Eq (DrawingF a f) where
  (==) = liftEq (==)
