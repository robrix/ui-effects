{-# LANGUAGE RankNTypes #-}
module Data.Functor.Algebraic where

import Control.Comonad.Cofree.Cofreer
import Control.Monad.Free.Freer
import Data.Foldable (fold)
import Data.Functor.Product
import Data.Functor.Sum

type Algebra functor result = functor result -> result
type Coalgebra functor seed = seed -> functor seed

-- | An algebra taking a function to apply to values inside the functor.
type FAlgebra functor result = forall x. (x -> result) -> functor x -> result

-- | A coalgebra taking a function to apply to values inside the functor.
type FCoalgebra functor seed = forall x. (seed -> x) -> seed -> functor x

sumAlgebra :: Algebra l a -> Algebra r a -> Algebra (Sum l r) a
sumAlgebra lAlgebra rAlgebra sum = case sum of
  InL l -> lAlgebra l
  InR r -> rAlgebra r

sumCoalgebra :: (Functor l, Functor r) => Coalgebra l a -> Coalgebra r b -> Coalgebra (Sum l r) (Either a b)
sumCoalgebra cl cr seed = case seed of
  Left l -> Left <$> InL (cl l)
  Right r -> Right <$> InR (cr r)

sumFCoalgebra :: FCoalgebra l a -> FCoalgebra r b -> Coalgebra (Sum l r) (Either a b)
sumFCoalgebra cl cr seed = case seed of
  Left l -> InL (cl Left l)
  Right r -> InR (cr Right r)


-- | Distributive law for Sum over FreerF.
distSumFreerF :: Sum (FreerF f a) (FreerF g a) c -> FreerF (Sum f g) a c
distSumFreerF s = case s of
  InL l -> case l of
    Pure a -> Pure a
    Free run f -> Free run (InL f)
  InR r -> case r of
    Pure a -> Pure a
    Free run f -> Free run (InR f)

-- | Distributive law for Sum over CofreerF.
distSumCofreerF :: Sum (CofreerF f a) (CofreerF g a) c -> CofreerF (Sum f g) a c
distSumCofreerF s = case s of
  InL (Cofree a t r) -> Cofree a t (InL r)
  InR (Cofree a t r) -> Cofree a t (InR r)


productCoalgebra :: Coalgebra l a -> Coalgebra r a -> Coalgebra (Product l r) a
productCoalgebra cl cr seed = Pair (cl seed) (cr seed)

productAlgebra :: (Functor l, Functor r) => Algebra l a -> Algebra r b -> Algebra (Product l r) (a, b)
productAlgebra al ar (Pair l r) = (al $ fst <$> l, ar $ snd <$> r)

productFAlgebra :: FAlgebra l a -> FAlgebra r b -> Algebra (Product l r) (a, b)
productFAlgebra al ar (Pair l r) = (al fst l, ar snd r)

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


annotating :: Functor f => Algebra f a -> Algebra f (Cofreer f a)
annotating algebra base = Cofreer (Cofree (algebra (extract <$> base)) id base)

annotatingBidi :: Functor f => (CofreerF f b a -> a) -> CofreerF f b (Cofreer f a) -> Cofreer f a
annotatingBidi algebra base = Cofreer (Cofree (algebra (extract <$> base)) id (tailF base))
