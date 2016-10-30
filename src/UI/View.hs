module UI.View where

import Control.Comonad
import Control.Comonad.Cofree
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import UI.Layout

-- Datatypes

data ViewF f
  = Text String
  | Label String
  | List [f]
  | Scroll (Maybe Axis) f
  deriving (Eq, Show, Functor)

data Axis = Horizontal | Vertical
  deriving (Eq, Show)

type View = Fix ViewF

type AView a = Cofree ViewF a


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
  Text s -> inset margins (resizeable (\ maxSize ->
    pure (fromMaybe <$> maybe measureString measureStringForWidth (width maxSize) s <*> maxSize)))
  Label s -> inset margins (pure (measureString s))
  Scroll _ child -> inset margins (resizeable (\ maxSize -> do
    childSize <- child
    pure (fromMaybe <$> childSize <*> maxSize)))
  List children -> inset margins (stack (intersperse (offset spacing (pure (Size 0 0))) children))
  where margins = Size 5 3
        spacing = Point 0 3


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
  (Nothing, Scroll _ sub) -> measure sub
  (Just size, Scroll axis sub) -> (size :<) . Scroll axis <$> measure sub
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

scroll :: Maybe Axis -> View -> View
scroll = (Fix .) . Scroll


-- Instances

instance Show1 ViewF where
  liftShowsPrec sp sl d view = case view of
    Text s -> showsUnaryWith showsPrec "Text" d s
    Label s -> showsUnaryWith showsPrec "Label" d s
    List l -> showsUnaryWith (liftShowsPrec sp sl) "List" d l
    Scroll a f -> showsBinaryWith showsPrec sp "Scroll" d a f
