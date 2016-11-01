{-# LANGUAGE GADTs #-}
module Control.Monad.Free.Freer where

import Control.Monad ((>=>))
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

instance Applicative (Freer f) where
   pure = Freer . Pure
   Freer g <*> a = case g of
     Pure f -> fmap f a
     Free t r -> Freer (Free ((<*> a) . t) r)

instance Monad (Freer f) where
  return = pure
  Freer g >>= f = case g of
    Pure a -> f a
    Free t r -> Freer (Free (t >=> f) r)
