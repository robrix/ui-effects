{-# LANGUAGE GADTs #-}
module Control.Comonad.Cofree.Cofreer where

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
