{-# LANGUAGE RankNTypes #-}
module Data.Functor.Algebraic where

import Control.Comonad.Cofree.Cofreer
import Control.Monad.Free.Freer
import Data.Foldable (fold)
import Data.Functor.Sum

type Algebra functor result = functor result -> result
type Coalgebra functor seed = seed -> functor seed

-- | A datatype for use as the interim structure in bidirectional computations represented as hylomorphisms.
data Bidi f a b = Bidi
  { bidiState :: a
  , bidiF :: f b }
  deriving (Eq, Foldable, Functor, Show)

setBidiF :: Bidi f a b -> f c -> Bidi f a c
setBidiF bidi a = bidi { bidiF = a }


sumAlgebra :: Algebra l a -> Algebra r a -> Algebra (Sum l r) a
sumAlgebra lAlgebra rAlgebra sum = case sum of
  InL l -> lAlgebra l
  InR r -> rAlgebra r


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

coannotating :: Functor f => Coalgebra f a -> Coalgebra f (Freer f a)
coannotating coalgebra seed = case runFreer seed of
  Pure a -> pure <$> coalgebra a
  Free run f -> run <$> f

annotatingBidi :: Algebra (Bidi (FreerF f c) b) a -> Algebra (Bidi (FreerF f c) b) (Cofreer (FreerF f c) a)
annotatingBidi algebra base = Cofreer (Cofree (algebra (extract <$> base)) id (bidiF base))


type CoalgebraFragment functor seed pure = (forall b x. seed -> (seed -> x -> b) -> functor x -> FreerF functor pure b)

liftBidiCoalgebra :: CoalgebraFragment f seed a -> Coalgebra (Bidi (FreerF f a) seed) (Bidi (FreerF f a) seed (Freer f a))
liftBidiCoalgebra fragment bidi = setBidiF bidi $ case bidiF bidi of
  Pure a -> Pure a
  Free runF functor -> fragment (bidiState bidi) (\ state -> Bidi state . runFreer . runF) functor

liftSumCoalgebra :: CoalgebraFragment l seed pure -> CoalgebraFragment r seed pure -> CoalgebraFragment (Sum l r) seed pure
liftSumCoalgebra lf rf state run sum = case sum of
  InL l -> hoistFreerF InL $ lf state run l
  InR r -> hoistFreerF InR $ rf state run r
