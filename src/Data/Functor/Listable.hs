module Data.Functor.Listable
( module LC
, Listable1(..)
) where

import Test.LeanCheck as LC (Listable(..))

class Listable1 l where
  liftTiers :: [[a]] -> [[l a]]
