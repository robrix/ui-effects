{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
module Control.Comonad.Cofree.Cofreer
( CofreerF(..)
, Cofreer(..)
, headF
, tailF
, cowrap
, unfold
, extract
, unwrap
) where

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

unfold :: Functor f => (b -> (a, f b)) -> b -> Cofreer f a
unfold f c = let (x, d) = f c in Cofreer (Cofree x (unfold f) d)


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


instance Foldable f => Foldable (Cofreer f) where
  foldMap f (Cofreer (Cofree a t r)) = mappend (f a) (foldMap (foldMap f . t) r)


type instance Base (Cofreer f a) = CofreerF f a

instance Recursive (Cofreer f a) where project = runCofreer
instance Corecursive (Cofreer f a) where embed = Cofreer


instance (Functor f, Show1 f) => Show2 (CofreerF f) where
  liftShowsPrec2 sp1 _ sp2 sa2 d (Cofree a t r) = showsTernaryWith sp1 (const showString) (liftShowsPrec sp2 sa2) "Cofree" d a "id" (t <$> r)
    where showsTernaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> String -> Int -> a -> b -> c -> ShowS
          showsTernaryWith sp1 sp2 sp3 name d x y z = showParen (d > 10) $ showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z

instance (Functor f, Show1 f) => Show1 (Cofreer f) where
  liftShowsPrec sp sa d (Cofreer c) = showsUnaryWith (liftShowsPrec2 sp sa (liftShowsPrec sp sa) (liftShowList sp sa)) "Cofreer" d c

instance (Functor f, Show (f (Cofreer f a)), Show a) => Show (Cofreer f a) where
  showsPrec d (Cofreer c) = showParen (d > 10) $ showString "Cofreer" . showChar ' ' . showsPrec 11 c

instance (Functor f, Show (f b), Show a) => Show (CofreerF f a b) where
  showsPrec d (Cofree a t r) = showParen (d > 10) $ showString "Cofree" . showChar ' ' . showsPrec 11 a . showString " id " . showsPrec 11 (t <$> r)
