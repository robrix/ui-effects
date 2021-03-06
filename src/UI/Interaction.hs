{-# LANGUAGE FlexibleContexts, GADTs #-}
module UI.Interaction where

import Control.Monad
import Control.Monad.Free.Freer
import Data.Functor.Union
import qualified Linear.Affine as Linear
import qualified Linear.V2 as Linear
import SDL.Event
import UI.Geometry

data InteractionF a f where
  Clickable :: Rect a -> f -> InteractionF a f

type Interaction a = Freer (InteractionF a)


clickable :: Rect a -> Interaction a b -> Interaction a b
clickable = (wrap .) . Clickable


runInteraction :: (InUnion fs IO, Real a) => Event -> Interaction a b -> Eff fs b
runInteraction event = iterFreerA (interactionAlgebra event)

interactionAlgebra :: (InUnion fs IO, Real a) => Event -> InteractionF a x -> (x -> Eff fs b) -> Eff fs b
interactionAlgebra event i run = case i of
  Clickable rect c -> do
    case eventPayload event of
      MouseButtonEvent m ->
        when (rect `containsPoint` toPoint (mouseButtonEventPos m)) $
          sendIO $ putStrLn $ if mouseButtonEventMotion m == Pressed
            then "down"
            else "up"
      _ -> pure ()
    run c
  where toPoint (Linear.P (Linear.V2 x y)) = Point (fromIntegral x) (fromIntegral y)
