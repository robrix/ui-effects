module UI.View where

import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Functor.Classes
import Data.Functor.Foldable

-- Datatypes

data ViewF f
  = Text String
  | List [f]
  | Scroll f
  deriving (Eq, Show, Functor)

type View = Fix ViewF

type AView a = Cofree ViewF a

data LayoutF a f
  = Inset a f
  | Divide a f f
  | Offset a f
  deriving (Eq, Show, Functor)

type Layout a = Free (LayoutF a)

inset :: a -> Layout a ()
inset d = liftF (Inset d ())

divide :: a -> Layout a ()
divide d = liftF (Divide d () ())

offset :: Real a => a -> Layout a ()
offset 0 = pure ()
offset d = liftF (Offset d ())


data Rect a = Rect { origin :: !(Point a), size :: !(Size a) }
  deriving (Eq, Show)
data Point a = Point { x :: !a, y :: !a }
  deriving (Eq, Show)
data Size a = Size { width :: !a, height :: !a }
  deriving (Eq, Show)

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
        stackSize = foldr (\ (Size w h :< _) (Size tw th) -> Size (max w tw) (th + h)) (Size 0 0)
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
