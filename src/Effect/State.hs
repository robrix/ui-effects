{-# LANGUAGE GADTs #-}
module Effect.State where

data State state result where
  Get :: State state state
  Put :: state -> State state ()
