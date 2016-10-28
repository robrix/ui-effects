module UI.View where

import Control.Comonad.Cofree
import Data.Functor.Foldable

-- Datatypes

data ViewF f
  = Text String
  | List [f]
  | Scroll f
  deriving (Eq, Show, Functor)

type View = Fix ViewF

type AView a = Cofree ViewF a

data Size = Size { width :: !Double, height :: !Double }
  deriving (Eq, Show)

measure :: View -> Maybe (AView Size)
measure = layout Nothing

fitTo :: Size -> View -> Maybe (AView Size)
fitTo = layout . Just

layout :: Maybe Size -> View -> Maybe (AView Size)
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

text :: String -> View
text = Fix . Text

list :: [View] -> View
list = Fix . List

scroll :: View -> View
scroll = Fix . Scroll
