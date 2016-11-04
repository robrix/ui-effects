{-# LANGUAGE FlexibleInstances, GADTs #-}
module UI.Layout where

import Control.Applicative
import Control.Comonad.Cofree.Cofreer
import Control.Monad.Free.Freer
import Data.Functor.Algebraic
import Data.Functor.Classes
import Data.Functor.Foldable hiding (unfold)
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Data.Semigroup
import UI.Geometry

data LayoutF a f where
  Inset :: Size a -> f -> LayoutF a f
  Offset :: Point a -> f -> LayoutF a f
  Resizeable :: (Size (Maybe a) -> f) -> LayoutF a f
  Measure :: f -> (Size a -> f) -> LayoutF a f
  deriving Functor

type Layout a = Freer (LayoutF a)
type ALayout a b = Cofreer (FreerF (LayoutF a) b)


-- Smart constructors

inset :: Size a -> Layout a b -> Layout a b
inset by = wrap . Inset by

offset :: Real a => Point a -> Layout a b -> Layout a b
offset (Point 0 0) = id
offset by = wrap . Offset by

resizeable :: (Size (Maybe a) -> Layout a b) -> Layout a b
resizeable = wrap . Resizeable

getMaxSize :: Layout a (Size (Maybe a))
getMaxSize = wrap (Resizeable pure)

measure :: Layout a b -> (Size a -> Layout a b) -> Layout a b
measure child = wrap . Measure child

newtype Stack a b = Stack { unStack :: Layout a b }

stack :: (Real a, Foldable t) => t (Layout a (Size a)) -> Layout a (Size a)
stack = unStack . foldMap Stack


-- Evaluation

measureLayout :: Real a => Layout a (Size a) -> Size a
measureLayout = fromMaybe (Size 0 0) . fitLayoutSize (pure Nothing)

fitLayoutSize :: Real a => Size (Maybe a) -> Layout a (Size a) -> Maybe (Size a)
fitLayoutSize = fitLayoutWith layoutSizeAlgebra

fitLayoutAndAnnotateSize :: Real a => Size (Maybe a) -> Layout a (Size a) -> ALayout a (Size a) (Maybe (Size a))
fitLayoutAndAnnotateSize = fitLayoutWith (annotatingBidi layoutSizeAlgebra)

layoutSizeAlgebra :: Real a => CofreerF (FreerF (LayoutF a) (Size a)) (Point a, Size (Maybe a)) (Maybe (Size a)) -> Maybe (Size a)
layoutSizeAlgebra c@(Cofree (origin, _) _ _) = size <$> layoutAlgebra (fmap (Rect origin) <$> c)


fitLayout :: Real a => Size (Maybe a) -> Layout a (Size a) -> Maybe (Rect a)
fitLayout = fitLayoutWith layoutAlgebra

fitLayoutAndAnnotate :: Real a => Size (Maybe a) -> Layout a (Size a) -> ALayout a (Size a) (Maybe (Rect a))
fitLayoutAndAnnotate = fitLayoutWith (annotatingBidi layoutAlgebra)

layoutAlgebra :: Real a => Algebra (Fitting LayoutF a) (Maybe (Rect a))
layoutAlgebra (Cofree (offset, maxSize) runC layout) = case layout of
  Pure size | maxSize `encloses` size -> Just (Rect offset (fromMaybe <$> size <*> maxSize))
  Free runF l -> case runC . runF <$> l of
    Inset by child -> Rect offset . (2 * by +) . size <$> child
    Offset by child -> Rect offset . (pointSize by +) . size <$> child
    Resizeable resize -> resize maxSize
    Measure child withMeasurement -> child >>= withMeasurement . size
  _ -> Nothing
  where maxSize `encloses` size = and (maybe (const True) (>=) <$> maxSize <*> size)


layoutRectanglesAlgebra :: Real a => Algebra (Fitting LayoutF a) [Rect a]
layoutRectanglesAlgebra c@(Cofree (_, maxSize) runC layout) = maybeToList (layoutAlgebra (listToMaybe <$> c)) <> case layout of
  Pure _ -> []
  Free runF l -> case runC . runF <$> l of
    Inset _ child -> child
    Offset _ child -> child
    Resizeable resize -> resize maxSize
    Measure child withMeasurement -> child >>= withMeasurement . size


type Fitting f a = Bidi (f a) (Size a) (Point a, Size (Maybe a))

fitLayoutWith :: Real a => Algebra (Fitting LayoutF a) b -> Size (Maybe a) -> Layout a (Size a) -> b
fitLayoutWith algebra maxSize layout = hylo algebra fittingCoalgebra (Point 0 0, maxSize, layout)

fittingCoalgebra :: Real a => Coalgebra (Fitting LayoutF a) (Point a, Size (Maybe a), Layout a (Size a))
fittingCoalgebra (offset, maxSize, layout) = Cofree (offset, maxSize) id $ case runFreer layout of
  Pure size -> Pure size
  Free run l -> Free id $ case run <$> l of
    Inset by child -> Inset by (addSizeToPoint offset by, subtractSize maxSize (2 * by), child)
    Offset by child -> Offset by (liftA2 (+) offset by, subtractSize maxSize (pointSize by), child)
    Resizeable resize -> Resizeable ((,,) offset maxSize . resize)
    Measure child withMeasurement -> Measure (offset, maxSize, child) ((,,) offset maxSize . withMeasurement)
  where subtractSize maxSize size = liftA2 (-) <$> maxSize <*> (Just <$> size)
        addSizeToPoint point (Size w h) = liftA2 (+) point (Point w h)


-- Instances

instance Real a => Monoid (Stack a (Size a)) where
  mempty = Stack (pure (Size 0 0))
  mappend a b = Stack $ do
    Size w1 h1 <- unStack a
    Size w2 h2 <- unStack b
    pure (Size (max w1 w2) (h1 + h2))

instance (Show a, Show b, Num a) => Show (LayoutF a b) where
  showsPrec d l = showParen (d > 10) $ case l of
    Inset by child -> showString "Inset" . showChar ' ' . showsPrec 11 by . showChar ' ' . showsPrec 11 child
    Offset by child -> showString "Offset" . showChar ' ' . showsPrec 11 by . showChar ' ' . showsPrec 11 child
    Resizeable with -> showString "Resizeable" . showChar ' ' . showsPrec 11 (with (pure Nothing))
    Measure child with -> showString "Measure" . showChar ' ' . showsPrec 11 child . showChar ' ' . showsPrec 11 (with (pure 0))

instance (Show a, Num a) => Show1 (LayoutF a) where
  liftShowsPrec sp _ d layout = case layout of
    Inset by child -> showsBinaryWith showsPrec sp "Inset" d by child
    Offset by child -> showsBinaryWith showsPrec sp "Offset" d by child
    Resizeable with -> showsUnaryWith showsConst "Resizeable" d (with (pure Nothing))
    Measure child with -> showsBinaryWith sp showsConst "Measure" d child (with (pure 0))
    where showsConst i = showParen True . (showString "const " .) . sp i
