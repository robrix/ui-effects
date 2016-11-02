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
type ALayout a b = Cofreer (FreerF (LayoutF a) b) b

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
measureLayout = fromMaybe (Size 0 0) . fitLayoutTo (pure Nothing)

fitLayout :: Real a => Size (Maybe a) -> Layout a (Size a) -> Maybe (Size a)
fitLayout = fitLayoutToAlgebra $ \ (Cofree maxSize runC layout) -> case layout of
  Pure size | maxSize `encloses` size -> Just (fromMaybe <$> size <*> maxSize)
  Free runF l -> case l of
    Inset by child -> (2 * by +) <$> runC (runF child)
    Offset by child -> (pointSize by +) <$> runC (runF child)
    Resizeable resize -> runC (runF (resize maxSize))
    Measure child withMeasurement -> runC (runF child) >>= runC . runF . withMeasurement
  _ -> Nothing
  where maxSize `encloses` size = and (maybe (const True) (>=) <$> maxSize <*> size)


fitLayoutAndAnnotate :: Real a => Size (Maybe a) -> Layout a (Size a) -> Maybe (ALayout a (Size a))
fitLayoutAndAnnotate = fitLayoutToAlgebra $ \ (Cofree maxSize runC layout) -> case layout of
  Pure size | maxSize `encloses` size -> Just ((fromMaybe <$> size <*> maxSize) `cowrap` Pure size)
  Free runF l -> case l of
    Inset by child -> do
      child <- runC (runF child)
      pure ((2 * by + extract child) `cowrap` liftFreerF (Inset by child))
    Offset by child -> do
      child <- runC (runF child)
      pure ((pointSize by + extract child) `cowrap` liftFreerF (Offset by child))
    Resizeable resize -> do
      computed <- runC (runF (resize maxSize))
      pure (extract computed `cowrap` liftFreerF (Resizeable (const computed)))
    Measure child withMeasurement -> do
      child <- runC (runF child)
      computed <- runC (runF (withMeasurement (extract child)))
      pure (extract computed `cowrap` liftFreerF (Measure child (const computed)))
  _ -> Nothing
  where maxSize `encloses` size = and (maybe (const True) (>=) <$> maxSize <*> size)

fitLayoutTo :: Real a => Size (Maybe a) -> Layout a (Size a) -> Maybe (Size a)
fitLayoutTo maxSize layout = case runFreer layout of
  Pure size | maxSize `encloses` size -> Just (fromMaybe <$> size <*> maxSize)
  Free toF (Inset inset rest) | maxSize `encloses` (2 * inset) -> (2 * inset +) <$> fitLayoutTo (subtractSize (2 * inset)) (toF rest)
  Free toF (Offset offset rest) | maxSize `encloses` pointSize offset -> (pointSize offset +) <$> fitLayoutTo (subtractSize (pointSize offset)) (toF rest)
  Free toF (Resizeable resize) -> fitLayoutTo maxSize (toF (resize maxSize))
  Free toF (Measure child withMeasurement) -> fitLayoutTo maxSize (toF (withMeasurement (measureLayout (toF child))))
  _ -> Nothing
  where maxSize `encloses` size = and (maybe (const True) (>=) <$> maxSize <*> size)
        subtractSize size = liftA2 (-) <$> maxSize <*> (Just <$> size)


fitLayoutToAlgebra :: Real a => (CofreerF (FreerF (LayoutF a) (Size a)) (Size (Maybe a)) b -> b) -> Size (Maybe a) -> Layout a (Size a) -> b
fitLayoutToAlgebra = curry . (`hylo` coalgebra)
  where coalgebra :: Real a => (Size (Maybe a), Layout a (Size a)) -> CofreerF (FreerF (LayoutF a) (Size a)) (Size (Maybe a)) (Size (Maybe a), Layout a (Size a))
        coalgebra (maxSize, layout) = Cofree maxSize id $ case runFreer layout of
          Pure size -> Pure size
          Free run l -> Free id $ case l of
            Inset by child -> Inset by (subtractSize maxSize (2 * by), run child)
            Offset by child -> Offset by (subtractSize maxSize (pointSize by), run child)
            Resizeable resize -> Resizeable ((,) maxSize . run . resize)
            Measure child withMeasurement -> Measure (maxSize, run child) ((,) maxSize . run . withMeasurement)

        subtractSize maxSize size = liftA2 (-) <$> maxSize <*> (Just <$> size)


extractAll :: Foldable f => Cofreer f a -> [a]
extractAll = foldMap pure

layoutRectangle :: Real a => ALayout a (Size a) -> ALayout a (Rect a)
layoutRectangle = unfold coalgebra . (,) (Point 0 0)
  where coalgebra :: Real a => (Point a, ALayout a (Size a)) -> (Rect a, FreerF (LayoutF a) (Rect a) (Point a, ALayout a (Size a)))
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
