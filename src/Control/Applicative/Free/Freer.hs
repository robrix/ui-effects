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

liftF :: g a -> Freer g a
liftF fa = Impure fa (Pure id)

iter :: Functor g => (g a -> a) -> Freer g a -> a
iter algebra = go
  where go (Pure a) = a
        go (Impure u q) = algebra (fmap (go . (`fmap` q) . flip ($)) u)

iterM :: (Functor g, Monad m) => (g (m a) -> m a) -> Freer g a -> m a
iterM algebra = go
  where go (Pure a) = pure a
        go (Impure u q) = algebra (fmap (go . (`fmap` q) . flip ($)) u)
