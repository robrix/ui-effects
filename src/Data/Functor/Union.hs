{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, PolyKinds, TypeOperators #-}
module Data.Functor.Union where

import Control.Monad.Free.Freer

data Union (fs :: [k -> *]) (a :: k) where
  Here :: f a -> Union (f ': fs) a
  There :: Union fs a -> Union (f ': fs) a

type Eff fs = Freer (Union fs)


runM :: Monad m => Freer m a -> m a
runM = iterFreerA (>>=)

strengthen :: Union '[f] a -> f a
strengthen (Here f) = f
strengthen _ = undefined

send :: InUnion fs f => f a -> Eff fs a
send = liftF . inj

sendIO :: InUnion fs IO => IO a -> Eff fs a
sendIO = send


-- Injection and projection

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


instance Functor f => Functor (Union '[f]) where
  fmap f = Here . fmap f . strengthen

instance (Functor f, Functor (Union (g ': hs))) => Functor (Union (f ': g ': hs)) where
  fmap f (Here e) = Here (fmap f e)
  fmap f (There t) = There (fmap f t)

instance Applicative f => Applicative (Union '[f]) where
  pure = Here . pure
  f <*> a = Here $ strengthen f <*> strengthen a

instance Monad m => Monad (Union '[m]) where
  return = pure
  m >>= f = Here $ strengthen m >>= strengthen . f
