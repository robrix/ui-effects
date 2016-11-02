{-# LANGUAGE FlexibleInstances, GADTs #-}
module UI.Layout where

import Control.Applicative
import Control.Comonad.Cofree.Cofreer
import Control.Monad.Free.Freer
import Data.Bifunctor
import Data.Functor.Foldable hiding (unfold)
import Data.Maybe (fromMaybe)
import UI.Geometry

data LayoutF a f where
  Inset :: Size a -> f -> LayoutF a f
  Offset :: Point a -> f -> LayoutF a f
  Resizeable :: (Size (Maybe a) -> f) -> LayoutF a f
  Measure :: f -> (Size a -> f) -> LayoutF a f
  deriving Functor

type Layout a = Freer (LayoutF a)
type ALayout a b = Cofreer (FreerF (LayoutF a) b)

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

measureLayout :: Real a => Layout a (Size a) -> Size a
measureLayout = fromMaybe (Size 0 0) . fitLayoutSize (pure Nothing)

fitLayoutSize :: Real a => Size (Maybe a) -> Layout a (Size a) -> Maybe (Size a)
fitLayoutSize = fitLayoutWith layoutSizeAlgebra

fitLayoutAndAnnotateSize :: Real a => Size (Maybe a) -> Layout a (Size a) -> ALayout a (Size a) (Maybe (Size a))
fitLayoutAndAnnotateSize = fitLayoutWith (annotatingBidi layoutSizeAlgebra)

layoutSizeAlgebra :: Real a => CofreerF (FreerF (LayoutF a) (Size a)) (Point a, Size (Maybe a)) (Maybe (Size a)) -> Maybe (Size a)
layoutSizeAlgebra (Cofree (_, maxSize) runC layout) = case layout of
  Pure size | maxSize `encloses` size -> Just (fromMaybe <$> size <*> maxSize)
  Free runF l -> case l of
    Inset by child -> (2 * by +) <$> runC (runF child)
    Offset by child -> (pointSize by +) <$> runC (runF child)
    Resizeable resize -> runC (runF (resize maxSize))
    Measure child withMeasurement -> runC (runF child) >>= runC . runF . withMeasurement
  _ -> Nothing
  where maxSize `encloses` size = and (maybe (const True) (>=) <$> maxSize <*> size)


fitLayout :: Real a => Size (Maybe a) -> Layout a (Size a) -> Maybe (Rect a)
fitLayout = fitLayoutWith layoutAlgebra

fitLayoutAndAnnotate :: Real a => Size (Maybe a) -> Layout a (Size a) -> ALayout a (Size a) (Maybe (Rect a))
fitLayoutAndAnnotate = fitLayoutWith (annotatingBidi layoutAlgebra)

layoutAlgebra :: Real a => CofreerF (FreerF (LayoutF a) (Size a)) (Point a, Size (Maybe a)) (Maybe (Rect a)) -> Maybe (Rect a)
layoutAlgebra (Cofree (offset, maxSize) runC layout) = case layout of
  Pure size | maxSize `encloses` size -> Just (Rect offset (fromMaybe <$> size <*> maxSize))
  Free runF l -> case l of
    Inset by child -> setOriginWith ((-) <$> sizeExtent by <*>) . setSizeWith (2 * by +) <$> runC (runF child)
    Offset by child -> setOriginWith ((-) <$> by <*>) . setSizeWith (pointSize by +) <$> runC (runF child)
    Resizeable resize -> runC (runF (resize maxSize))
    Measure child withMeasurement -> runC (runF child) >>= runC . runF . withMeasurement . size
  _ -> Nothing
  where maxSize `encloses` size = and (maybe (const True) (>=) <$> maxSize <*> size)
        setOriginWith f rect = rect { origin = f (origin rect) }
        setSizeWith f rect = rect { size = f (size rect) }


fitLayoutWith :: Real a => (CofreerF (FreerF (LayoutF a) (Size a)) (Point a, Size (Maybe a)) b -> b) -> Size (Maybe a) -> Layout a (Size a) -> b
fitLayoutWith algebra maxSize layout = hylo algebra coalgebra (Point 0 0, maxSize, layout)
  where coalgebra :: Real a => (Point a, Size (Maybe a), Layout a (Size a)) -> CofreerF (FreerF (LayoutF a) (Size a)) (Point a, Size (Maybe a)) (Point a, Size (Maybe a), Layout a (Size a))
        coalgebra (offset, maxSize, layout) = Cofree (offset, maxSize) id $ case runFreer layout of
          Pure size -> Pure size
          Free run l -> Free id $ case l of
            Inset by child -> Inset by (addSizeToPoint offset by, subtractSize maxSize (2 * by), run child)
            Offset by child -> Offset by (liftA2 (+) offset by, subtractSize maxSize (pointSize by), run child)
            Resizeable resize -> Resizeable ((,,) offset maxSize . run . resize)
            Measure child withMeasurement -> Measure (offset, maxSize, run child) ((,,) offset maxSize . run . withMeasurement)

        subtractSize maxSize size = liftA2 (-) <$> maxSize <*> (Just <$> size)
        addSizeToPoint point (Size w h) = liftA2 (+) point (Point w h)


layoutRectangle :: Real a => ALayout a (Size a) (Size a) -> ALayout a (Rect a) (Rect a)
layoutRectangle = unfold coalgebra . (,) (Point 0 0)
  where coalgebra :: Real a => (Point a, ALayout a (Size a) (Size a)) -> (Rect a, FreerF (LayoutF a) (Rect a) (Point a, ALayout a (Size a) (Size a)))
        coalgebra (offset, Cofreer (Cofree size runC layout)) =
          let rect = Rect offset size
              assign origin = bimap (Rect origin) ((,) origin . runC) layout
          in (,) rect $ case layout of
            Pure _ -> assign offset
            Free _ l -> case l of
              Offset by _ -> assign (liftA2 (+) offset by)
              _ -> assign offset


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
