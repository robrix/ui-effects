{-# LANGUAGE GADTs #-}
module Control.Monad.Free.Freer where

import Data.Bifunctor

data FreerF f a b where
  Pure :: a -> FreerF f a b
  Free :: (x -> b) -> f x -> FreerF f a b

newtype Freer f a = Freer { runFreer :: FreerF f a (Freer f a) }


-- Instances

instance Bifunctor (FreerF f) where
  bimap f _ (Pure a) = Pure (f a)
  bimap _ g (Free t r) = Free (g . t) r

instance Functor (FreerF f a) where
  fmap = second

instance Functor (Freer f) where
  fmap f = Freer . bimap f (fmap f) . runFreer
