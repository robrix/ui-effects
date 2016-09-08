{-# LANGUAGE GADTs #-}
module Control.Applicative.Free.Freer where

data Freer g a where
  Pure :: a -> Freer g a
  Impure :: g x -> Freer g (x -> a) -> Freer g a

instance Functor (Freer g) where
  fmap f (Pure x)     = Pure (f x)
  fmap f (Impure u q) = Impure u (fmap (f .) q)

instance Applicative (Freer g) where
  pure = Pure
  Pure f     <*> x = fmap f x
  Impure u q <*> x = Impure u (flip <$> q <*> x)
