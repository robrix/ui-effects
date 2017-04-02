{-# LANGUAGE DataKinds, GADTs, PolyKinds, TypeOperators #-}
module Data.Functor.Union where

data Union (fs :: [k -> *]) (a :: k) where
  Here :: f a -> Union (f ': fs) a
  There :: Union fs a -> Union (f ': fs) a
