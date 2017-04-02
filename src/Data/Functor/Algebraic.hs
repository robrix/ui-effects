{-# LANGUAGE RankNTypes #-}
module Data.Functor.Algebraic where

import Control.Comonad.Cofree.Cofreer
import Control.Monad.Free.Freer as Freer
import Control.Monad.Trans.Free.Freer as FreerF
import Data.Foldable (fold)
import Data.Functor.Foldable (project)
import Data.Functor.Sum

type Algebra functor result = functor result -> result
type Coalgebra functor seed = seed -> functor seed

-- | A datatype for use as the interim structure in bidirectional computations represented as hylomorphisms.
data Bidi f a b = Bidi
  { bidiState :: a
  , bidiF :: f b }
  deriving (Eq, Foldable, Functor, Show)


sumAlgebra :: Algebra l a -> Algebra r a -> Algebra (Sum l r) a
sumAlgebra lAlgebra rAlgebra sum = case sum of
  InL l -> lAlgebra l
  InR r -> rAlgebra r

hoistSum :: (l a -> l' b) -> (r a -> r' b) -> Sum l r a -> Sum l' r' b
hoistSum ifl ifr sum = case sum of
  InL l -> InL (ifl l)
  InR r -> InR (ifr r)


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
annotating algebra base = Cofree (algebra (extract <$> base)) base id

coannotating :: Functor f => Coalgebra f a -> Coalgebra f (Freer f a)
coannotating coalgebra seed = case seed of
  Freer.Return a -> pure <$> coalgebra a
  f `Freer.Then` run -> run <$> f

annotatingBidi :: Algebra (Bidi (FreerF f c) b) a -> Algebra (Bidi (FreerF f c) b) (Cofreer (FreerF f c) a)
annotatingBidi algebra base = Cofree (algebra (extract <$> base)) (bidiF base) id


type CoalgebraFragment functor seed pure = (forall b x. seed -> (seed -> x -> b) -> functor x -> FreerF functor pure b)

liftBidiCoalgebra :: CoalgebraFragment f seed a -> Coalgebra (Bidi (FreerF f a) seed) (Bidi (FreerF f a) seed (Freer f a))
liftBidiCoalgebra fragment (Bidi state f) = Bidi state $ case f of
  FreerF.Return a -> FreerF.Return a
  functor `FreerF.Then` runF -> fragment state (\ state -> Bidi state . project . runF) functor

liftSumCoalgebra :: CoalgebraFragment l seed pure -> CoalgebraFragment r seed pure -> CoalgebraFragment (Sum l r) seed pure
liftSumCoalgebra lf rf state run sum = case sum of
  InL l -> hoistFreerF InL $ lf state run l
  InR r -> hoistFreerF InR $ rf state run r
