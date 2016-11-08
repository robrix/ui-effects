{-# LANGUAGE DataKinds, GADTs, RankNTypes #-}
module GL.Setup where

import Control.Monad.Free.Freer
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

data Shader where
  Vertex :: Shader.Shader Shader.Vertex -> Shader
  Fragment :: Shader.GLSLValue a => Shader.Shader a -> Shader

data SetupF a where
  Flag :: Flag -> Bool -> SetupF ()
  SetDepthFunc :: Func -> SetupF ()
  SetBlendFactors :: Factor -> Factor -> SetupF ()
  SetClearColour :: Real n => Linear.V4 n -> SetupF ()
  BindArray :: (Foldable v, GLScalar n) => [v n] -> SetupF (GLArray n)
  BuildProgram :: [Shader] -> SetupF GLProgram
  RunIO :: IO a -> SetupF a
  Uniform :: Shader.GLSLValue a => String -> SetupF (Shader.Var (Shader.Shader a))

type Setup = Freer SetupF

enable :: Flag -> Setup ()
enable = liftF . (`Flag` True)

disable :: Flag -> Setup ()
disable = liftF . (`Flag` False)

setClearColour :: Linear.V4 Float -> Setup ()
setClearColour = liftF . SetClearColour

setDepthFunc :: Func -> Setup ()
setDepthFunc = liftF . SetDepthFunc

setBlendFactors :: Factor -> Factor -> Setup ()
setBlendFactors = (liftF .) . SetBlendFactors

bindArray :: (Foldable v, GLScalar n) => [v n] -> Setup (GLArray n)
bindArray = liftF . BindArray

buildProgram :: [Shader] -> Setup GLProgram
buildProgram = liftF . BuildProgram

setupIO :: IO a -> Setup a
setupIO = liftF . RunIO

uniform :: Shader.GLSLValue a => String -> Setup (Shader.Var (Shader.Shader a))
uniform = liftF . Uniform

runSetup :: Setup a -> IO a
runSetup = iterFreerA $ \ rest s -> case s of
  Flag f b -> do
    toggle b $ case f of
      DepthTest -> GL_DEPTH_TEST
      Blending -> GL_BLEND
    checkingGLError (rest ())
  SetDepthFunc f -> do
    glDepthFunc $ case f of
      Less -> GL_LESS
    checkingGLError (rest ())
  SetBlendFactors source destination -> do
    glBlendFunc (factor source) (factor destination)
    checkingGLError (rest ())
  SetClearColour (Linear.V4 r g b a) -> do
    glClearColor (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
    checkingGLError (rest ())
  BindArray vertices -> withVertices vertices (checkingGLError . rest)
  BuildProgram shaders -> withBuiltProgram (compileShader <$> shaders) (checkingGLError . rest)
  RunIO io -> io >>= rest
  Uniform s -> rest (Shader.Uniform s)
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

compileShader :: Shader -> (GLenum, String)
compileShader (Vertex shader) = (GL_VERTEX_SHADER, Shader.toGLSL (Shader.elaborateVertexShader shader))
compileShader (Fragment shader) = (GL_FRAGMENT_SHADER, Shader.toGLSL (Shader.elaborateShader shader))
