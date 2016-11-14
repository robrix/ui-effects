module Data.Functor.Listable
( Listable(..)
, cons0
, cons1
, cons2
, cons3
, cons4
, cons5
, cons6
, Listable1(..)
, tiers1
, Listable2(..)
, tiers2
) where

import Test.LeanCheck

class Listable1 l where
  liftTiers :: [[a]] -> [[l a]]

tiers1 :: (Listable a, Listable1 l) => [[l a]]
tiers1 = liftTiers tiers


class Listable2 l where
  liftTiers2 :: [[a]] -> [[b]] -> [[l a b]]

tiers2 :: (Listable a, Listable b, Listable2 l) => [[l a b]]
tiers2 = liftTiers2 tiers tiers
