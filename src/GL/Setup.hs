{-# LANGUAGE DataKinds, GADTs, RankNTypes #-}
module GL.Setup where

import Control.Action
import Control.Monad.Free
import GL.Array
import GL.Exception
import GL.Program
import GL.Scalar
import qualified GL.Shader as Shader
import Graphics.GL.Core41
import Graphics.GL.Types
import qualified Linear.V4 as Linear
import Prelude hiding (IO)

data Flag = DepthTest
data Func = Less

data Shader a where
  Vertex :: Shader.Shader 'Shader.Vertex a -> Shader a
  Fragment :: Shader.Shader 'Shader.Fragment a -> Shader a

data SetupF a where
  Flag :: Flag -> Bool -> SetupF ()
  SetDepthFunc :: Func -> SetupF ()
  SetClearColour :: Real n => Linear.V4 n -> SetupF ()
  BindArray :: (Foldable v, GLScalar n) => [v n] -> SetupF (GLArray n)
  BuildProgram :: [Shader a] -> SetupF GLProgram
  RunIO :: IO a -> SetupF a

type Setup = Free (Action SetupF)

enable :: Flag -> Setup ()
enable = liftF . liftAction . (`Flag` True)

disable :: Flag -> Setup ()
disable = liftF . liftAction . (`Flag` False)

setClearColour :: Linear.V4 Float -> Setup ()
setClearColour = liftF . liftAction . SetClearColour

setDepthFunc :: Func -> Setup ()
setDepthFunc = liftF . liftAction . SetDepthFunc

bindArray :: (Foldable v, GLScalar n) => [v n] -> Setup (GLArray n)
bindArray = liftF . liftAction . BindArray

buildProgram :: [Shader a] -> Setup GLProgram
buildProgram = liftF . liftAction . BuildProgram

setupIO :: IO a -> Setup a
setupIO = liftF . liftAction . RunIO

runSetup :: Setup a -> IO a
runSetup = iterM $ \ s -> case s of
  Action (Flag f b) rest -> do
    toggle b $ case f of
      DepthTest -> GL_DEPTH_TEST
    checkingGLError (rest ())
  Action (SetDepthFunc f) rest -> do
    glDepthFunc $ case f of
      Less -> GL_LESS
    checkingGLError (rest ())
  Action (SetClearColour (Linear.V4 r g b a)) rest -> do
    glClearColor (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
    checkingGLError (rest ())
  Action (BindArray vertices) rest -> withVertices vertices (checkingGLError . rest)
  Action (BuildProgram shaders) rest -> withBuiltProgram (compileShader <$> shaders) (checkingGLError . rest)
  Action (RunIO io) rest -> io >>= rest
  where toggle b = if b then glEnable else glDisable

compileShader :: Shader a -> (GLenum, String)
compileShader (Vertex shader) = (GL_VERTEX_SHADER, Shader.toGLSL shader)
compileShader (Fragment shader) = (GL_FRAGMENT_SHADER, Shader.toGLSL shader)
