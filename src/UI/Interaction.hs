{-# LANGUAGE GADTs #-}
module UI.Interaction where

import Control.Monad.Free.Freer
import SDL.Event
import UI.Geometry

data InteractionF a f where
  Clickable :: Rect a -> f -> InteractionF a f

type Interaction a = Freer (InteractionF a)


clickable :: Rect a -> Interaction a b -> Interaction a b
clickable = (wrap .) . Clickable


runInteraction :: Event -> Interaction a b -> IO b
runInteraction event = iterFreerA (interactionAlgebra event)

interactionAlgebra :: Event -> (x -> IO b) -> InteractionF a x -> IO b
interactionAlgebra _ run i = case i of
  Clickable _ c -> run c
