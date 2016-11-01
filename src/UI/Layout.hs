{-# LANGUAGE FlexibleInstances, GADTs #-}
module UI.Layout where

import Control.Applicative
import Control.Monad.Free.Church
import Control.Monad.Free (Free (Pure, Free))
import Data.Maybe (fromMaybe)
import UI.Geometry

data LayoutF a f where
  Inset :: Size a -> f -> LayoutF a f
  Offset :: Point a -> f -> LayoutF a f
  Resizeable :: (Size (Maybe a) -> f) -> LayoutF a f
  Measure :: f -> (Size a -> f) -> LayoutF a f
  deriving Functor

type Layout a = F (LayoutF a)

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
fitLayoutTo maxSize layout = case fromF layout of
  Pure size | maxSize `encloses` size -> Just (fromMaybe <$> size <*> maxSize)
  Free (Inset inset rest) | maxSize `encloses` (2 * inset) -> (2 * inset +) <$> fitLayoutTo (subtractSize (2 * inset)) (toF rest)
  Free (Offset offset rest) | maxSize `encloses` pointSize offset -> (pointSize offset +) <$> fitLayoutTo (subtractSize (pointSize offset)) (toF rest)
  Free (Resizeable resize) -> fitLayoutTo maxSize (toF (resize maxSize))
  Free (Measure child withMeasurement) -> fitLayoutTo maxSize (toF (withMeasurement (measureLayout (toF child))))
  _ -> Nothing
  where maxSize `encloses` size = and (maybe (const True) (>=) <$> maxSize <*> size)
        subtractSize size = liftA2 (-) <$> maxSize <*> (Just <$> size)


-- Instances

instance Real a => Monoid (Stack a (Size a)) where
  mempty = Stack (pure (Size 0 0))
  mappend a b = Stack $ do
    Size w1 h1 <- unStack a
    Size w2 h2 <- unStack b
    pure (Size (max w1 w2) (h1 + h2))
