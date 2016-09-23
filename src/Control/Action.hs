{-# LANGUAGE GADTs #-}
module Control.Action where

-- | An action on a value in some type constructor. Essentially Co-Yoneda. 🎩 Jeremy Gibbons.
data Action f a where
  Action :: f r -> (r -> a) -> Action f a


instance Functor (Action f) where
  fmap f (Action functor run) = Action functor (f . run)
