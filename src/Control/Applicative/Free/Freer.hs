{-# LANGUAGE GADTs #-}
module Control.Applicative.Free.Freer where

data Freer g a where
  Pure :: a -> Freer g a
  Impure :: Freer g (b -> a) -> g b -> Freer g a

instance Functor (Freer g) where
  fmap f (Pure x)     = Pure (f x)
  fmap f (Impure q u) = Impure (fmap (f .) q) u

instance Applicative (Freer g) where
  pure = Pure
  Pure f     <*> x = fmap f x
  Impure q u <*> x = Impure (flip <$> q <*> x) u

liftF :: g a -> Freer g a
liftF = Impure (Pure id)

iter :: Functor g => (g a -> a) -> Freer g a -> a
iter algebra = go
  where go (Pure a) = a
        go (Impure q u) = algebra (fmap (go . (`fmap` q) . flip ($)) u)

iterM :: (Functor g, Monad m) => (g (m a) -> m a) -> Freer g a -> m a
iterM algebra = go
  where go (Pure a) = pure a
        go (Impure q u) = algebra (fmap (go . (`fmap` q) . flip ($)) u)
