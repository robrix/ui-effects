{-# LANGUAGE GADTs #-}
module UI.Interaction where

import UI.Geometry

data InteractionF a f where
  Clickable :: Rect a -> f -> InteractionF a f
