{-# LANGUAGE FlexibleInstances #-}
module UI.Layout where

import Control.Applicative
import Control.Monad.Free.Church
import Control.Monad.Free (Free (Pure, Free))
import Data.Maybe (fromMaybe)
import Data.Semigroup

data LayoutF a f
  = Inset (Size a) f
  | Offset (Point a) f
  | Resizeable (Size (Maybe a) -> f)
  deriving Functor

type Layout a = F (LayoutF a)

inset :: Size a -> Layout a b -> Layout a b
inset = (wrap .) . Inset

offset :: Real a => Point a -> Layout a b -> Layout a b
offset (Point 0 0) = id
offset by = wrap . Offset by

resizeable :: (Size (Maybe a) -> Layout a b) -> Layout a b
resizeable = wrap . Resizeable

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
  _ -> Nothing
  where maxSize `encloses` size = and (maybe (const True) (>=) <$> maxSize <*> size)
        subtractSize size = liftA2 (-) <$> maxSize <*> (Just <$> size)


-- Geometry

data Rect a = Rect { origin :: !(Point a), size :: !(Size a) }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)
data Point a = Point { x :: !a, y :: !a }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

pointSize :: Point a -> Size a
pointSize (Point x y) = Size x y

data Size a = Size { width :: !a, height :: !a }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

encloses :: Ord a => Size a -> Size a -> Bool
encloses a b = and ((>=) <$> a <*> b)


-- Instances

instance Applicative Size where
  pure a = Size a a
  Size f g <*> Size a b = Size (f a) (g b)

instance Num a => Num (Size a) where
  fromInteger = pure . fromInteger
  abs = liftA abs
  signum = liftA signum
  negate = liftA negate
  (+) = liftA2 (+)
  (*) = liftA2 (*)

instance Semigroup a => Semigroup (Size a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Size a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Real a => Monoid (Stack a (Size a)) where
  mempty = Stack (pure (Size 0 0))
  mappend a b = Stack $ do
    Size w1 h1 <- unStack a
    Size w2 h2 <- unStack b
    pure (Size (max w1 w2) (h1 + h2))
