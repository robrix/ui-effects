{-# LANGUAGE GADTs #-}
module Control.Comonad.Cofree.Cofreer where

data CofreerF f a b where
  Cofree :: a -> (x -> b) -> f x -> CofreerF f a b

newtype Cofreer f a = Cofreer { runCofreer :: CofreerF f a (Cofreer f a) }
