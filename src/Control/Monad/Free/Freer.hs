{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes, TypeFamilies, UndecidableInstances #-}
module Control.Monad.Free.Freer
( FreerF(..)
, Freer(..)
, liftFreerF
, iter
, iterA
, hoistFreer
, hoistFreerF
, liftF
, wrap
) where

import Control.Monad ((>=>))
import Control.Monad.Free.Class
import Data.Bifunctor
import Data.Functor.Classes
import Data.Functor.Foldable

data FreerF f a b where
  Pure :: a -> FreerF f a b
  Free :: (x -> b) -> f x -> FreerF f a b

liftFreerF :: f b -> FreerF f a b
liftFreerF = Free id


newtype Freer f a = Freer { runFreer :: FreerF f a (Freer f a) }

iter :: Functor f => (f a -> a) -> Freer f a -> a
iter algebra = cata $ \ r -> case r of
  Pure a -> a
  Free t r -> algebra (t <$> r)

iterA :: (Functor f, Applicative m) => (f (m a) -> m a) -> Freer f a -> m a
iterA algebra = cata $ \ r -> case r of
  Pure a -> pure a
  Free t r -> algebra (t <$> r)


hoistFreer :: (forall a. f a -> g a) -> Freer f b -> Freer g b
hoistFreer f = go
  where go = Freer . fmap go . hoistFreerF f . runFreer

hoistFreerF :: (forall a. f a -> g a) -> FreerF f b c -> FreerF g b c
hoistFreerF f r = case r of
  Pure a -> Pure a
  Free t r -> Free t (f r)


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


instance Foldable f => Foldable (FreerF f a) where
  foldMap f g = case g of
    Pure _ -> mempty
    Free t r -> foldMap (f . t) r

instance Foldable f => Foldable (Freer f) where
  foldMap f = foldMap (foldMap f) . runFreer


instance Traversable f => Traversable (FreerF f a) where
  traverse f g = case g of
    Pure a -> pure (Pure a)
    Free t r -> Free id <$> traverse (f . t) r

instance Traversable f => Traversable (Freer f) where
  traverse f = go
    where go g = case runFreer g of
            Pure a -> Freer . Pure <$> f a
            Free t r -> Freer . Free id <$> traverse (go . t) r


type instance Base (Freer f a) = FreerF f a

instance Recursive (Freer f a) where project = runFreer
instance Corecursive (Freer f a) where embed = Freer


instance Show1 f => Show2 (FreerF f) where
  liftShowsPrec2 sp1 _ sp2 sa2 d f = case f of
    Pure a -> showsUnaryWith sp1 "Pure" d a
    Free t r -> showsUnaryWith (liftShowsPrec (\ i -> sp2 i . t) (sa2 . fmap t)) "Free" d r

instance Show1 f => Show1 (Freer f) where
  liftShowsPrec sp sa d (Freer c) = showsUnaryWith (liftShowsPrec2 sp sa (liftShowsPrec sp sa) (liftShowList sp sa)) "Freer" d c

instance (Functor f, Show (f (Freer f a)), Show a) => Show (Freer f a) where
  showsPrec d (Freer c) = showParen (d > 10) $ showString "Freer" . showChar ' ' . showsPrec 11 c

instance (Functor f, Show (f b), Show a) => Show (FreerF f a b) where
  showsPrec d f = case f of
    Pure a -> showParen (d > 10) $ showString "Pure" . showChar ' ' . showsPrec 11 a
    Free t r -> showParen (d > 10) $ showString "Free" . showChar ' ' . showsPrec 11 (t <$> r)
