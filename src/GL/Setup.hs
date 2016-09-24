{-# LANGUAGE GADTs #-}
module GL.Setup where

import Control.Action
import Control.Applicative.Free.Freer
import Graphics.GL.Core41
import qualified Linear.V4 as Linear

data Flag = DepthTest
data Func = Less

data SetupF a where
  Flag :: Flag -> Bool -> SetupF ()
  SetDepthFunc :: Func -> SetupF ()
  SetClearColour :: Real n => Linear.V4 n -> SetupF ()

type Setup = Freer (Action SetupF)

enable :: Flag -> Setup ()
enable = liftF . liftAction . (`Flag` True)

disable :: Flag -> Setup ()
disable = liftF . liftAction . (`Flag` False)

setClearColour :: Linear.V4 Float -> Setup ()
setClearColour = liftF . liftAction . SetClearColour

setDepthFunc :: Func -> Setup ()
setDepthFunc = liftF . liftAction . SetDepthFunc

runSetup :: Setup () -> IO ()
runSetup = iterM $ \ s -> case s of
  Action (Flag f b) rest -> do
    toggle b $ case f of
      DepthTest -> GL_DEPTH_TEST
    rest ()
  Action (SetDepthFunc f) rest -> do
    glDepthFunc $ case f of
      Less -> GL_LESS
    rest ()
  Action (SetClearColour (Linear.V4 r g b a)) rest -> do
    glClearColor (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
    rest ()
  where toggle b = if b then glEnable else glDisable
