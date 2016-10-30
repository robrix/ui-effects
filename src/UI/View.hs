{-# LANGUAGE FlexibleInstances #-}
module UI.View where

import Control.Applicative
import Control.Comonad
import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.List (intersperse)
import Data.Semigroup

-- Datatypes

data ViewF f
  = Text String
  | Label String
  | List [f]
  | Scroll f
  deriving (Eq, Show, Functor)

type View = Fix ViewF

type AView a = Cofree ViewF a

data LayoutF a f
  = Inset (Size a) f
  | Offset (Point a) f
  | Bounded (Maybe (Size a) -> f)
  deriving Functor

type Layout a = Free (LayoutF a)

inset :: Size a -> Layout a b -> Layout a b
inset = (wrap .) . Inset

offset :: Real a => Point a -> Layout a b -> Layout a b
offset (Point 0 0) = id
offset by = wrap . Offset by

bounded :: (Maybe (Size a) -> Layout a b) -> Layout a b
bounded = wrap . Bounded


newtype Stack a b = Stack { unStack :: Layout a b }

stack :: (Real a, Foldable t) => t (Layout a (Size a)) -> Layout a (Size a)
stack = unStack . foldMap Stack


measureString :: Num a => String -> Size a
measureString s = Size (fromIntegral (length s) * fontW) lineH
  where (fontW, fontH) = (5, 8)
        lineH = fontH + 5

measureStringForWidth :: Real a => a -> String -> Size a
measureStringForWidth maxW s = Size maxW (height line * fromInteger (ceiling (toRational (length s) / (toRational maxW / toRational (width char)))))
  where char = Size 5 8
        line = char + Size 10 5

layoutView :: Real a => View -> Layout a (Size a)
layoutView = cata $ \ view -> case view of
  Text s -> inset margins (bounded (pure . ($ s) . maybe measureString (measureStringForWidth . width)))
  Label s -> inset margins (pure (measureString s))
  Scroll child -> inset margins (bounded (maybe child pure))
  List children -> inset margins (stack (intersperse (offset spacing (pure (Size 0 0))) children))
  where margins = Size 5 3
        spacing = Point 0 3


measureLayout :: Real a => Layout a (Size a) -> Size a
measureLayout = iter $ \ layout -> case layout of
  Inset inset size -> size + (2 * inset)
  Offset offset size -> size + pointSize offset
  Bounded f -> f Nothing

fitLayoutTo :: Real a => Size a -> Layout a (Size a) -> Maybe (Size a)
fitLayoutTo maxSize layout = case layout of
  Pure a -> Just a
  Free (Inset inset rest) -> (2 * inset +) <$> fitLayoutTo (maxSize - (2 * inset)) rest
  Free (Offset offset rest) -> (pointSize offset +) <$> fitLayoutTo (maxSize - pointSize offset) rest
  Free (Bounded f) -> fitLayoutTo maxSize (f (Just maxSize))

runLayout :: Real a => Maybe (Size a) -> Layout a (Size a) -> Maybe (Size a)
runLayout maxSize = (Just .) . iter $ \ layout -> case layout of
  Inset inset size -> size + (2 * inset)
  Offset (Point byx byy) (Size w h) -> Size (w + byx) (h + byy)
  Bounded f -> f maxSize


data Rect a = Rect { origin :: !(Point a), size :: !(Size a) }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)
data Point a = Point { x :: !a, y :: !a }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

pointSize :: Point a -> Size a
pointSize (Point x y) = Size x y

data Size a = Size { width :: !a, height :: !a }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

measure :: Real a => View -> Maybe (AView (Size a))
measure = layout Nothing

fitTo :: Real a => Size a -> View -> Maybe (AView (Size a))
fitTo = layout . Just

layout :: Real a => Maybe (Size a) -> View -> Maybe (AView (Size a))
layout size view = case (size, unfix view) of
  (Nothing, Text s) -> Just (measureString s :< Text s)
  (Just size, Text s) -> Just (size :< Text s)
  (Nothing, Label s) -> Just (measureString s :< Label s)
  (Nothing, List as) -> (\ as -> stackSize as :< List as) <$> traverse measure as
  (Nothing, Scroll sub) -> measure sub
  (Just size, Scroll sub) -> (size :<) . Scroll <$> measure sub
  (Just maxSize, _) -> do
    laidOut@(minSize :< _) <- measure view
    if width maxSize >= width minSize && height maxSize >= height minSize
      then Just laidOut
      else Nothing
  where stackSize :: Real a => [AView (Size a)] -> Size a
        stackSize = foldr (\ each into -> Size max (+) <*> into <*> each) (Size 0 0) . fmap extract


-- Smart constructors

text :: String -> View
text = Fix . Text

list :: [View] -> View
list = Fix . List

scroll :: View -> View
scroll = Fix . Scroll


-- Instances

instance Show1 ViewF where
  liftShowsPrec sp sl d view = case view of
    Text s -> showsUnaryWith showsPrec "Text" d s
    Label s -> showsUnaryWith showsPrec "Label" d s
    List l -> showsUnaryWith (liftShowsPrec sp sl) "List" d l
    Scroll f -> showsUnaryWith sp "Scroll" d f

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
