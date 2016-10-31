{-# LANGUAGE TypeOperators #-}
module Data.Functor.Sum where

data (f :+: g) a = L (f a) | R (g a)

runSum :: (f a -> r) -> (g a -> r) -> (f :+: g) a -> r
runSum f g sum = case sum of
  L a -> f a
  R a -> g a


instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f = runSum (L . fmap f) (R . fmap f)
