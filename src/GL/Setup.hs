{-# LANGUAGE DataKinds, GADTs, LambdaCase #-}
module GL.Setup where

import Control.Action
import Control.Monad.Free.Freer
import GL.Array
import GL.Program
import GL.Scalar
import GL.Shader
import Graphics.GL.Core41
import Graphics.GL.Types
import qualified Graphics.Shader as Graphics
import qualified Linear.V4 as Linear

data Flag = DepthTest
data Func = Less

data Shader a where
  Vertex :: Graphics.Shader 'Graphics.Vertex a -> Shader a
  Fragment :: Graphics.Shader 'Graphics.Fragment a -> Shader a

data SetupF a where
  Flag :: Flag -> Bool -> SetupF ()
  SetDepthFunc :: Func -> SetupF ()
  SetClearColour :: Real n => Linear.V4 n -> SetupF ()
  BindArray :: (Foldable v, GLScalar n) => [v n] -> SetupF (GLArray n)
  BuildProgram :: [Shader a] -> SetupF GLProgram

type Setup = Freer (Action SetupF)

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

runSetup :: Setup a -> IO a
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
  Action (BindArray vertices) rest -> withVertices vertices rest
  Action (BuildProgram shaders) rest -> withBuiltProgram (compileShader <$> shaders) rest
  where toggle b = if b then glEnable else glDisable

compileShader :: Shader a -> (GLenum, String)
compileShader (Vertex shader) = (GL_VERTEX_SHADER, toGLSL shader)
compileShader (Fragment shader) = (GL_FRAGMENT_SHADER, toGLSL shader)
