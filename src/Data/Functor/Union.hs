{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, MultiParamTypeClasses, PolyKinds, TypeOperators #-}
module Data.Functor.Union where

data Union (fs :: [k -> *]) (a :: k) where
  Here :: f a -> Union (f ': fs) a
  There :: Union fs a -> Union (f ': fs) a

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
