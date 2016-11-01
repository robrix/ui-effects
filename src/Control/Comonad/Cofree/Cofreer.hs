{-# LANGUAGE GADTs #-}
module Control.Comonad.Cofree.Cofreer where

data CofreerF f a b where
  Cofree :: a -> (x -> b) -> f x -> CofreerF f a b

headF :: CofreerF f a b -> a
headF (Cofree a _ _) = a

tailF :: Functor f => CofreerF f a b -> f b
tailF (Cofree _ t r) = t <$> r


newtype Cofreer f a = Cofreer { runCofreer :: CofreerF f a (Cofreer f a) }
