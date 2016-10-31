{-# LANGUAGE TypeFamilies #-}
module Control.Applicative.Free.Class where

class Applicative f => ApplicativeFree f where
  type Underlying f :: * -> *

  liftAp :: Underlying f a -> f a
