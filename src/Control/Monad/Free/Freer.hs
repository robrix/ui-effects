{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, TypeFamilies #-}
module Control.Monad.Free.Freer where

import Control.Monad ((>=>))
import Control.Monad.Free.Class
import Data.Bifunctor
import Data.Functor.Foldable

data FreerF f a b where
  Pure :: a -> FreerF f a b
  Free :: (x -> b) -> f x -> FreerF f a b

newtype Freer f a = Freer { runFreer :: FreerF f a (Freer f a) }

iter :: Functor f => (f a -> a) -> Freer f a -> a
iter algebra = cata $ \ r -> case r of
  Pure a -> a
  Free t r -> algebra (t <$> r)


-- Instances

instance Bifunctor (FreerF f) where
  bimap f g r = case r of
    Pure a -> Pure (f a)
    Free t r -> Free (g . t) r

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

instance MonadFree f (Freer f) where
  wrap = Freer . Free id

type instance Base (Freer f a) = FreerF f a

instance Recursive (Freer f a) where project = runFreer
instance Corecursive (Freer f a) where embed = Freer
