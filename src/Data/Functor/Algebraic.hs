{-# LANGUAGE RankNTypes #-}
module Data.Functor.Algebraic where

import Control.Comonad.Cofree.Cofreer
import Control.Comonad.Trans.Cofree
import Control.Monad.Free.Freer as Freer
import Control.Monad.Trans.Free.Freer as FreerF
import Data.Foldable (fold)
import Data.Functor.Foldable (project)

type Algebra functor result = functor result -> result
type Coalgebra functor seed = seed -> functor seed

-- | A datatype for use as the interim structure in bidirectional computations represented as hylomorphisms.
type Bidi = CofreeF


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
annotatingBidi algebra base = Cofree (algebra (extract <$> base)) (tailF base) id


type CoalgebraFragment functor seed pure = (forall b x. seed -> (seed -> x -> b) -> functor x -> FreerF functor pure b)

liftBidiCoalgebra :: CoalgebraFragment f seed a -> Coalgebra (Bidi (FreerF f a) seed) (Bidi (FreerF f a) seed (Freer f a))
liftBidiCoalgebra fragment (state :< f) = state :< case f of
  FreerF.Return a -> FreerF.Return a
  functor `FreerF.Then` runF -> fragment state (\ state -> (state :<) . project . runF) functor
