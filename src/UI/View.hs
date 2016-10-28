module UI.View where

import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Functor.Foldable

-- Datatypes

data ViewF f
  = Text String
  | List [f]
  | Scroll f
  deriving Functor

type View a = Free ViewF a

type AView a = Cofree ViewF a

data Rectangle = Rectangle { origin :: !Point, size :: !Size }
data Point = Point { x :: !Double, y :: !Double }
data Size = Size { width :: !Double, height :: !Double }

measure :: Fix ViewF -> Maybe (AView Size)
measure = layout Nothing

fitTo :: Size -> Fix ViewF -> Maybe (AView Size)
fitTo = layout . Just

layout :: Maybe Size -> Fix ViewF -> Maybe (AView Size)
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
  where stackSize :: [AView Size] -> Size
        stackSize = foldr (\ (Size w h :< _) (Size tw th) -> Size (max w tw) (th + h)) (Size 0 0)
        (fontW, fontH) = (5, 8)
        lineH = fontH + 5


-- Smart constructors

text :: String -> View ()
text = wrap . Text

list :: [View a] -> View a
list = wrap . List

scroll :: View a -> View a
scroll = wrap . Scroll
