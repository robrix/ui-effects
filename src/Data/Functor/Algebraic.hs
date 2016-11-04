module Data.Functor.Algebraic where

type Algebra functor result = functor result -> result
type Coalgebra functor seed = seed -> functor seed
