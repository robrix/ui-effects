{-# LANGUAGE FlexibleContexts, GADTs #-}
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
