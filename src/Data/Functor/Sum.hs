{-# LANGUAGE TypeOperators #-}
module Data.Functor.Sum where

data (f :+: g) a = L (f a) | R (g a)
