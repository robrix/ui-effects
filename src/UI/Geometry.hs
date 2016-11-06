module UI.Geometry where

import Control.Applicative (liftA, liftA2)
import Data.Functor.Classes
import Data.Semigroup

data Rect a = Rect { origin :: !(Point a), size :: !(Size a) }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)
data Point a = Point { x :: !a, y :: !a }
  deriving (Eq, Foldable, Functor, Ord, Traversable)

pointSize :: Point a -> Size a
pointSize (Point x y) = Size x y

data Size a = Size { width :: !a, height :: !a }
  deriving (Eq, Foldable, Functor, Ord, Traversable)

encloses :: Ord a => Size a -> Size a -> Bool
encloses a b = and ((>=) <$> a <*> b)

sizeExtent :: Size a -> Point a
sizeExtent (Size w h) = Point w h


-- Instances

instance Applicative Point where
  pure a = Point a a
  Point f g <*> Point a b = Point (f a) (g b)

instance Show1 Point where
  liftShowsPrec sp _ d (Point x y) = showsBinaryWith sp sp "Point" d x y

instance Show a => Show (Point a) where
  showsPrec = liftShowsPrec showsPrec showList


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

instance Show1 Size where
  liftShowsPrec sp _ d (Size w h) = showsBinaryWith sp sp "Show" d w h

instance Show a => Show (Size a) where
  showsPrec = liftShowsPrec showsPrec showList
