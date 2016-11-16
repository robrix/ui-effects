{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes, TypeFamilies #-}
module Control.Monad.Free.Freer
( FreerF(..)
, Freer(..)
, liftFreerF
, iter
, iterA
, iterFreer
, iterFreerA
, hoistFreer
, hoistFreerF
, liftF
, wrap
) where

import Control.Monad ((>=>))
import Control.Monad.Free.Class hiding (liftF)
import Data.Bifunctor
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.Functor.Listable

data FreerF f a b where
  Pure :: a -> FreerF f a b
  Free :: (x -> b) -> f x -> FreerF f a b

liftFreerF :: f b -> FreerF f a b
liftFreerF = Free id


newtype Freer f a = Freer { runFreer :: FreerF f a (Freer f a) }

iter :: Functor f => (f a -> a) -> Freer f a -> a
iter algebra = iterFreer ((algebra .) . fmap)

iterA :: (Functor f, Applicative m) => (f (m a) -> m a) -> Freer f a -> m a
iterA algebra = cata $ \ r -> case r of
  Pure a -> pure a
  Free t r -> algebra (t <$> r)

iterFreer :: (forall x. (x -> a) -> f x -> a) -> Freer f a -> a
iterFreer algebra = cata $ \ r -> case r of
  Pure a -> a
  Free t r -> algebra t r

iterFreerA :: Applicative m => (forall x. (x -> m a) -> f x -> m a) -> Freer f a -> m a
iterFreerA algebra = cata $ \ r -> case r of
  Pure a -> pure a
  Free t r -> algebra t r

hoistFreer :: (forall a. f a -> g a) -> Freer f b -> Freer g b
hoistFreer f = go
  where go = Freer . fmap go . hoistFreerF f . runFreer

hoistFreerF :: (forall a. f a -> g a) -> FreerF f b c -> FreerF g b c
hoistFreerF f r = case r of
  Pure a -> Pure a
  Free t r -> Free t (f r)


liftF :: f a -> Freer f a
liftF = Freer . Free pure


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
    Free t r -> showsBinaryWith (const showString) (liftShowsPrec (\ i -> sp2 i . t) (sa2 . fmap t)) "Free" d "id" r

instance (Show1 f, Show a) => Show1 (FreerF f a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Show1 f => Show1 (Freer f) where
  liftShowsPrec sp sa d (Freer c) = showsUnaryWith (liftShowsPrec2 sp sa (liftShowsPrec sp sa) (liftShowList sp sa)) "Freer" d c

instance (Show1 f, Show a, Show b) => Show (FreerF f a b) where
  showsPrec = liftShowsPrec showsPrec showList

instance (Show1 f, Show a) => Show (Freer f a) where
  showsPrec = liftShowsPrec showsPrec showList

instance Eq1 f => Eq2 (FreerF f) where
  liftEq2 eqA eqB f1 f2 = case (f1, f2) of
    (Pure a1, Pure a2) -> eqA a1 a2
    (Free t1 r1, Free t2 r2) -> liftEq (\ x1 x2 -> eqB (t1 x1) (t2 x2)) r1 r2
    _ -> False

instance (Eq1 f, Eq a) => Eq1 (FreerF f a) where
  liftEq = liftEq2 (==)

instance (Eq1 f, Eq a, Eq b) => Eq (FreerF f a b) where
  (==) = liftEq (==)

instance Eq1 f => Eq1 (Freer f) where
  liftEq eqA = go where go (Freer f1) (Freer f2) = liftEq2 eqA go f1 f2

instance (Eq1 f, Eq a) => Eq (Freer f a) where
  (==) = liftEq (==)

instance Listable1 f => Listable2 (FreerF f) where
  liftTiers2 t1 t2 = liftCons1 t1 Pure \/ liftCons1 (liftTiers t2) (Free id)

instance (Listable a, Listable1 f) => Listable1 (FreerF f a) where
  liftTiers = liftTiers2 tiers

instance (Listable a, Listable b, Listable1 f) => Listable (FreerF f a b) where
  tiers = liftTiers tiers

instance Listable1 f => Listable1 (Freer f) where
  liftTiers t1 = go
    where go = liftCons1 (liftTiers2 t1 go) Freer

instance (Listable a, Listable1 f) => Listable (Freer f a) where
  tiers = liftTiers tiers
