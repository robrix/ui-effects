module Data.Functor.Listable
( Listable(..)
, Listable1(..)
, tiers1
) where

import Test.LeanCheck

class Listable1 l where
  liftTiers :: [[a]] -> [[l a]]

tiers1 :: (Listable a, Listable1 l) => [[l a]]
tiers1 = liftTiers tiers
