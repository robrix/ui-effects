module Data.Functor.Algebraic where

import Control.Comonad.Cofree.Cofreer
import Control.Monad.Free.Freer
import Data.Functor.Sum

type Algebra functor result = functor result -> result
type Coalgebra functor seed = seed -> functor seed

type Bidi functor pure state = CofreerF (FreerF functor pure) state

sumAlgebra :: Algebra l a -> Algebra r a -> Algebra (Sum l r) a
sumAlgebra lAlgebra rAlgebra sum = case sum of
  InL l -> lAlgebra l
  InR r -> rAlgebra r
