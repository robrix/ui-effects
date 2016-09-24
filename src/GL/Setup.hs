{-# LANGUAGE GADTs #-}
module GL.Setup where

import Control.Action
import Control.Applicative.Free.Freer
import qualified Linear.V4 as Linear

data Flag = DepthTest
data Func = Less

data SetupF a where
  Flag :: Flag -> Bool -> SetupF ()
  DepthFunc :: Func -> SetupF ()
  SetClearColour :: Linear.V4 n -> SetupF ()

type Setup = Freer (Action SetupF)

enable :: Flag -> Setup ()
enable = liftF . liftAction . (`Flag` True)

disable :: Flag -> Setup ()
disable = liftF . liftAction . (`Flag` False)

setClearColour :: Linear.V4 Float -> Setup ()
setClearColour = liftF . liftAction . SetClearColour
