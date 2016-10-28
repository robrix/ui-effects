module UI.View where

import Control.Applicative
import Control.Comonad
import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.Semigroup

-- Datatypes

data ViewF f
  = Text String
  | List [f]
  | Scroll f
  deriving (Eq, Show, Functor)

type View = Fix ViewF

type AView a = Cofree ViewF a

data LayoutF a f
  = Inset (Size a) f
  | Divide a f f
  | Offset (Point a) f
  deriving (Eq, Show, Functor)

type Layout a = Free (LayoutF a)

inset :: Size a -> Layout a ()
inset d = liftF (Inset d ())

divide :: a -> Layout a ()
divide d = liftF (Divide d () ())

offset :: Real a => Point a -> Layout a ()
offset (Point 0 0) = pure ()
offset by = liftF (Offset by ())


layoutView :: Real a => View -> Layout a (Size a)
layoutView = cata $ \ view -> case view of
  Text s -> pure (Size (fromIntegral (length s) * 5) 13)
  Scroll child -> do
    inset 5
    child
  List children -> do
    inset 5
    foldl layoutChild (pure (Size 0 0)) children
  where layoutChild from each = do
          Size w h <- from
          offset (Point 0 h)
          Size w' h' <- each
          pure (Size (max w w') h')

runLayout :: Real a => Layout a (Size a) -> Size a
runLayout = iter $ \ layout -> case layout of
  Inset inset size -> size + (2 * inset)
  Offset (Point byx byy) (Size w h) -> Size (w + byx) (h + byy)
  Divide _ (Size w1 h1) (Size w2 h2) -> Size (max w1 w2) (h1 + h2)


data Rect a = Rect { origin :: !(Point a), size :: !(Size a) }
  deriving (Eq, Foldable, Functor, Show, Traversable)
data Point a = Point { x :: !a, y :: !a }
  deriving (Eq, Foldable, Functor, Show, Traversable)

pointSize :: Point a -> Size a
pointSize (Point x y) = Size x y

data Size a = Size { width :: !a, height :: !a }
  deriving (Eq, Foldable, Functor, Show, Traversable)

measure :: Real a => View -> Maybe (AView (Size a))
measure = layout Nothing

fitTo :: Real a => Size a -> View -> Maybe (AView (Size a))
fitTo = layout . Just

layout :: Real a => Maybe (Size a) -> View -> Maybe (AView (Size a))
layout size view = case (size, unfix view) of
  (Nothing, Text s) -> Just (Size (fromIntegral (length s) * fontW) lineH :< Text s)
  (Just size, Text s) -> Just (size :< Text s)
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
        (fontW, fontH) = (5, 8)
        lineH = fontH + 5


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
