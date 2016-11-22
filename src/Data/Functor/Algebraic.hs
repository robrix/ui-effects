module Data.Functor.Algebraic where

import Control.Comonad.Cofree.Cofreer
import Control.Monad.Free.Freer
import Data.Foldable (fold)
import Data.Functor.Product
import Data.Functor.Sum

type Algebra functor result = functor result -> result
type Coalgebra functor seed = seed -> functor seed

type Bidi functor pure state = CofreerF (FreerF functor pure) state

sumAlgebra :: Algebra l a -> Algebra r a -> Algebra (Sum l r) a
sumAlgebra lAlgebra rAlgebra sum = case sum of
  InL l -> lAlgebra l
  InR r -> rAlgebra r

productCoalgebra :: Coalgebra l a -> Coalgebra r a -> Coalgebra (Product l r) a
productCoalgebra cl cr seed = Pair (cl seed) (cr seed)

liftL :: Functor l => Freer l a -> Freer (Sum l r) a
liftL (Freer f) = case f of
  Free t r -> wrapL (liftL . t <$> r)
  Pure a -> pure a

liftR :: Functor r => Freer r a -> Freer (Sum l r) a
liftR  (Freer f) = case f of
  Free t r -> wrapR (liftR . t <$> r)
  Pure a -> pure a

wrapL :: l (Freer (Sum l r) a) -> Freer (Sum l r) a
wrapL = wrap . InL

wrapR :: r (Freer (Sum l r) a) -> Freer (Sum l r) a
wrapR = wrap . InR

liftFL :: l a -> Freer (Sum l r) a
liftFL = liftF . InL

liftFR :: r a -> Freer (Sum l r) a
liftFR = liftF . InR

collect :: (Foldable f, Functor f) => Algebra f a -> Algebra f [a]
collect algebra c = wrapAlgebra ((++ fold c) . pure) head algebra c

wrapAlgebra :: Functor f => (a -> b) -> (b -> a) -> Algebra f a -> Algebra f b
wrapAlgebra into outOf algebra = into . algebra . fmap outOf
