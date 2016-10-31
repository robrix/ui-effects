{-# LANGUAGE TypeFamilies #-}
module Control.Applicative.Free.Class where

import Control.Applicative.Free as Ap (Ap, liftAp)
import Control.Monad.Free

class Applicative f => ApplicativeFree f where
  type Underlying f :: * -> *

  liftAp :: Underlying f a -> f a

instance ApplicativeFree (Ap f) where
  type Underlying (Ap f) = f
  liftAp = Ap.liftAp

instance Functor f => ApplicativeFree (Free f) where
  type Underlying (Free f) = f
  liftAp = liftF
