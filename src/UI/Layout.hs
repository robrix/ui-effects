{-# LANGUAGE FlexibleInstances, GADTs #-}
module UI.Layout where

import Control.Applicative
import Control.Comonad
import Control.Comonad.Cofree.Cofreer
import Control.Monad.Free.Freer
import Data.Foldable
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


fitLayoutTo' :: Real a => Size (Maybe a) -> Layout a (Size a) -> Maybe (ALayout a (Size a))
fitLayoutTo' maxSize layout = case runFreer layout of
  Pure size | maxSize `encloses` size ->
    Just ((fromMaybe <$> size <*> maxSize) `cowrap` Pure size)
  Free run (Inset by child) | maxSize `encloses` (2 * by) -> do
    child <- fitLayoutTo' (subtractSize (2 * by)) (run child)
    pure ((2 * by + extract child) `cowrap` liftFreerF (Inset by child))
  Free run (Offset by child) | maxSize `encloses` pointSize by -> do
    child <- fitLayoutTo' (subtractSize (pointSize by)) (run child)
    pure ((pointSize by + extract child) `cowrap` liftFreerF (Offset by child))
  Free run (Resizeable resize) -> do
    computed <- fitLayoutTo' maxSize (run (resize maxSize))
    pure (extract computed `cowrap` liftFreerF (Resizeable (const computed)))
  Free run (Measure child withMeasurement) -> do
    child <- fitLayoutTo' (pure Nothing) (run child)
    computed <- fitLayoutTo' maxSize (run (withMeasurement (extract child)))
    pure (extract computed `cowrap` liftFreerF (Measure child (const computed)))
  _ -> Nothing
  where maxSize `encloses` size = and (maybe (const True) (>=) <$> maxSize <*> size)
        subtractSize size = liftA2 (-) <$> maxSize <*> (Just <$> size)


extractAll :: Foldable f => Cofreer f a -> [a]
extractAll = fold . fmap pure

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
