{-# LANGUAGE GADTs #-}
module Control.Monad.Free.Freer where

data Freer g a where
  Pure :: a -> Freer g a
  Impure :: g x -> (x -> Freer g a) -> Freer g a

instance Functor (Freer g) where
  fmap f (Pure x)     = Pure (f x)
  fmap f (Impure u q) = Impure u (fmap f . q)

instance Applicative (Freer g) where
  pure = Pure
  Pure f     <*> x = fmap f x
  Impure u q <*> x = Impure u ((<*> x) . q)

instance Monad (Freer g) where
  return = Pure
  Pure x      >>= k = k x
  Impure u k' >>= k = Impure u (k' >>> k)
    where (>>>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
          f >>> g = (>>= g) . f

wrap :: g (Freer g a) -> Freer g a
wrap fa = Impure fa id

liftF :: g a -> Freer g a
liftF fa = Impure fa Pure

iter :: Functor g => (g a -> a) -> Freer g a -> a
iter algebra = go
  where go (Pure a) = a
        go (Impure u q) = algebra (fmap (go . q) u)

iterM :: (Functor g, Monad m) => (g (m a) -> m a) -> Freer g a -> m a
iterM algebra = go
  where go (Pure a) = pure a
        go (Impure u q) = algebra (fmap (go . q) u)
