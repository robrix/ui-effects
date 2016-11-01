{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses #-}
module Control.Comonad.Cofree.Cofreer where

import Control.Comonad
import Control.Comonad.Cofree.Class
import Data.Bifunctor

data CofreerF f a b where
  Cofree :: a -> (x -> b) -> f x -> CofreerF f a b

headF :: CofreerF f a b -> a
headF (Cofree a _ _) = a

tailF :: Functor f => CofreerF f a b -> f b
tailF (Cofree _ t r) = t <$> r


newtype Cofreer f a = Cofreer { runCofreer :: CofreerF f a (Cofreer f a) }


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
