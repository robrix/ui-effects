{-# LANGUAGE GADTs #-}
module UI.Interaction where

import Control.Monad.Free.Freer
import UI.Geometry

data InteractionF a f where
  Clickable :: Rect a -> f -> InteractionF a f

type Interaction a = Freer (InteractionF a)


clickable :: Rect a -> Interaction a b -> Interaction a b
clickable = (wrap .) . Clickable
