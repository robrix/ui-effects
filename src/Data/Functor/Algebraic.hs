module Data.Functor.Algebraic where

import Control.Comonad.Cofree.Cofreer
import Control.Monad.Free.Freer

type Algebra functor result = functor result -> result
type Coalgebra functor seed = seed -> functor seed

type Bidi functor pure state = CofreerF (FreerF functor pure) state
