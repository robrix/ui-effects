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

data Flag = DepthTest | Blending
data Func = Less
data Factor
  = Zero
  | One
  | DestinationAlpha
  | DestinationColour
  | OneMinusDestinationAlpha
  | OneMinusDestinationColour
  | SourceAlpha
  | SourceAlphaSaturate
  | SourceColour
  | OneMinusSourceAlpha
  | OneMinusSourceColour

data Shader a where
  Vertex :: Shader.Shader a -> Shader a
  Fragment :: Shader.Shader a -> Shader a

data SetupF a where
  Flag :: Flag -> Bool -> SetupF ()
  SetDepthFunc :: Func -> SetupF ()
  SetBlendFactors :: Factor -> Factor -> SetupF ()
  SetClearColour :: Real n => Linear.V4 n -> SetupF ()
  BindArray :: (Foldable v, GLScalar n) => [v n] -> SetupF (GLArray n)
  BuildProgram :: Shader.GLSLValue a => [Shader a] -> SetupF GLProgram
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

setBlendFactors :: Factor -> Factor -> Setup ()
setBlendFactors = ((liftF .) liftAction .) . SetBlendFactors

bindArray :: (Foldable v, GLScalar n) => [v n] -> Setup (GLArray n)
bindArray = liftF . liftAction . BindArray

buildProgram :: Shader.GLSLValue a => [Shader a] -> Setup GLProgram
buildProgram = liftF . liftAction . BuildProgram

setupIO :: IO a -> Setup a
setupIO = liftF . liftAction . RunIO

runSetup :: Setup a -> IO a
runSetup = iterM $ \ s -> case s of
  Action (Flag f b) rest -> do
    toggle b $ case f of
      DepthTest -> GL_DEPTH_TEST
      Blending -> GL_BLEND
    checkingGLError (rest ())
  Action (SetDepthFunc f) rest -> do
    glDepthFunc $ case f of
      Less -> GL_LESS
    checkingGLError (rest ())
  Action (SetBlendFactors source destination) rest -> do
    glBlendFunc (factor source) (factor destination)
    checkingGLError (rest ())
  Action (SetClearColour (Linear.V4 r g b a)) rest -> do
    glClearColor (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
    checkingGLError (rest ())
  Action (BindArray vertices) rest -> withVertices vertices (checkingGLError . rest)
  Action (BuildProgram shaders) rest -> withBuiltProgram (compileShader <$> shaders) (checkingGLError . rest)
  Action (RunIO io) rest -> io >>= rest
  where toggle b = if b then glEnable else glDisable
        factor f = case f of
          Zero -> GL_ZERO
          One -> GL_ONE
          DestinationAlpha -> GL_DST_ALPHA
          DestinationColour -> GL_DST_COLOR
          OneMinusDestinationAlpha -> GL_ONE_MINUS_DST_ALPHA
          OneMinusDestinationColour -> GL_ONE_MINUS_DST_COLOR
          SourceAlpha -> GL_SRC_ALPHA
          SourceAlphaSaturate -> GL_SRC_ALPHA_SATURATE
          SourceColour -> GL_SRC_COLOR
          OneMinusSourceAlpha -> GL_ONE_MINUS_SRC_ALPHA
          OneMinusSourceColour -> GL_ONE_MINUS_SRC_COLOR

compileShader :: Shader.GLSLValue a => Shader a -> (GLenum, String)
compileShader (Vertex shader) = (GL_VERTEX_SHADER, Shader.toGLSL shader)
compileShader (Fragment shader) = (GL_FRAGMENT_SHADER, Shader.toGLSL shader)
