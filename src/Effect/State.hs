{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, TypeOperators #-}
module Effect.State where

import Control.Monad.Free.Freer
import Data.Functor.Union

data State state result where
  Get :: State state state
  Put :: state -> State state ()

get :: InUnion fs (State state) => Freer (Union fs) state
get = inj Get `Then` return

put :: InUnion fs (State state) => state -> Freer (Union fs) ()
put value = inj (Put value) `Then` return

runState :: Freer (Union (State s ': fs)) a -> s -> Freer (Union fs) (a, s)
runState = iterFreer (\ union yield s -> case union of
  Here Get -> yield s s
  Here (Put s) -> yield () s
  There fs -> fs `Then` flip yield s) . fmap ((return .) . (,))
