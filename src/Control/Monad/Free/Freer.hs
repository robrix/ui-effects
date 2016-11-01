{-# LANGUAGE GADTs #-}
module Control.Monad.Free.Freer where

data FreerF f a b where
  Pure :: a -> FreerF f a b
  Free :: (x -> b) -> f x -> FreerF f a b

newtype Freer f a = Freer { runFreer :: FreerF f a (Freer f a) }
