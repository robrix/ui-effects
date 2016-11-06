{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes, TypeFamilies #-}
module Control.Comonad.Cofree.Cofreer
( CofreerF(..)
, Cofreer(..)
, headF
, tailF
, cowrap
, coiter
, unfold
, hoistCofreer
, hoistCofreerF
, annotating
, annotatingBidi
, extract
, unwrap
) where

import Control.Arrow ((&&&))
import Control.Comonad
import Control.Comonad.Cofree.Class
import Data.Bifunctor
import Data.Functor.Classes
import Data.Functor.Foldable hiding (unfold)

data CofreerF f a b where
  Cofree :: a -> (x -> b) -> f x -> CofreerF f a b

headF :: CofreerF f a b -> a
headF (Cofree a _ _) = a

tailF :: Functor f => CofreerF f a b -> f b
tailF (Cofree _ t r) = t <$> r


newtype Cofreer f a = Cofreer { runCofreer :: CofreerF f a (Cofreer f a) }

infixr 5 `cowrap`
cowrap :: a -> f (Cofreer f a) -> Cofreer f a
cowrap a r = Cofreer (Cofree a id r)

coiter :: Functor f => (b -> f b) -> b -> Cofreer f b
coiter f = unfold (id &&& f)

unfold :: Functor f => (b -> (a, f b)) -> b -> Cofreer f a
unfold f c = let (x, d) = f c in Cofreer (Cofree x (unfold f) d)


hoistCofreer :: (forall a. f a -> g a) -> Cofreer f b -> Cofreer g b
hoistCofreer f = go
  where go = Cofreer . fmap go . hoistCofreerF f . runCofreer

hoistCofreerF :: (forall a. f a -> g a) -> CofreerF f b c -> CofreerF g b c
hoistCofreerF f (Cofree a t r) = Cofree a t (f r)


annotating :: Functor f => (f a -> a) -> f (Cofreer f a) -> Cofreer f a
annotating algebra base = Cofreer (Cofree (algebra (extract <$> base)) id base)

annotatingBidi :: Functor f => (CofreerF f b a -> a) -> CofreerF f b (Cofreer f a) -> Cofreer f a
annotatingBidi algebra base = Cofreer (Cofree (algebra (extract <$> base)) id (tailF base))


-- Instances

instance Bifunctor (CofreerF f) where
  bimap f g (Cofree a t r) = Cofree (f a) (g . t) r

instance Functor (CofreerF f a) where
  fmap = second

instance Functor (Cofreer f) where
  fmap f = Cofreer . bimap f (fmap f) . runCofreer

instance Comonad (Cofreer f) where
  extract (Cofreer (Cofree a _ _)) = a
  extend f c@(Cofreer (Cofree _ t r)) = Cofreer (Cofree (f c) (extend f . t) r)

instance Functor f => ComonadCofree f (Cofreer f) where
  unwrap = tailF . runCofreer


instance Foldable f => Foldable (CofreerF f a) where
  foldMap f (Cofree _ t r) = foldMap (f . t) r

instance Foldable f => Foldable (Cofreer f) where
  foldMap f (Cofreer c) = mappend (f (headF c)) (foldMap (foldMap f) c)


instance Traversable f => Traversable (CofreerF f a) where
  traverse f (Cofree a t r) = Cofree a id <$> traverse (f . t) r

instance Traversable f => Traversable (Cofreer f) where
  traverse f (Cofreer (Cofree a t r)) = cowrap <$> f a <*> traverse (traverse f . t) r


type instance Base (Cofreer f a) = CofreerF f a

instance Recursive (Cofreer f a) where project = runCofreer
instance Corecursive (Cofreer f a) where embed = Cofreer


instance Show1 f => Show2 (CofreerF f) where
  liftShowsPrec2 sp1 _ sp2 sa2 d (Cofree a t r) = showsTernaryWith sp1 (const showString) (liftShowsPrec (\ i -> sp2 i . t) (sa2 . fmap t)) "Cofree" d a "id" r
    where showsTernaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> String -> Int -> a -> b -> c -> ShowS
          showsTernaryWith sp1 sp2 sp3 name d x y z = showParen (d > 10) $ showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z

instance (Show1 f, Show a) => Show1 (CofreerF f a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Show1 f => Show1 (Cofreer f) where
  liftShowsPrec sp sa d (Cofreer c) = showsUnaryWith (liftShowsPrec2 sp sa (liftShowsPrec sp sa) (liftShowList sp sa)) "Cofreer" d c

instance (Show1 f, Show a, Show b) => Show (CofreerF f a b) where
  showsPrec = liftShowsPrec showsPrec showList

instance (Show1 f, Show a) => Show (Cofreer f a) where
  showsPrec = liftShowsPrec showsPrec showList

instance Eq1 f => Eq2 (CofreerF f) where
  liftEq2 eqA eqB (Cofree a1 t1 r1) (Cofree a2 t2 r2) = eqA a1 a2 && liftEq (\ x1 x2 -> eqB (t1 x1) (t2 x2)) r1 r2

instance (Eq1 f, Eq a) => Eq1 (CofreerF f a) where
  liftEq = liftEq2 (==)

instance (Eq1 f, Eq a, Eq b) => Eq (CofreerF f a b) where
  (==) = liftEq (==)

