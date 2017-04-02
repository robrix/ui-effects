{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, MultiParamTypeClasses, PolyKinds, TypeOperators #-}
module Data.Functor.Union where

import Control.Monad.Free.Freer

data Union (fs :: [k -> *]) (a :: k) where
  Here :: f a -> Union (f ': fs) a
  There :: Union fs a -> Union (f ': fs) a

type Eff fs = Freer (Union fs)


runM :: Monad m => Freer m a -> m a
runM = iterFreerA (>>=)

lower :: Union '[f] a -> f a
lower (Here f) = f
lower _ = undefined

class InUnion (fs :: [k -> *]) (f :: k -> *) where
  inj :: f a -> Union fs a
  prj :: Union fs a -> Maybe (f a)

instance {-# OVERLAPPABLE #-} InUnion (f ': fs) f where
  inj = Here
  prj (Here f) = Just f
  prj _ = Nothing

instance {-# OVERLAPPABLE #-} InUnion fs f => InUnion (g ': fs) f where
  inj = There . inj
  prj (There fs) = prj fs
  prj _ = Nothing
