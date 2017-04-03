{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, RankNTypes, RecordWildCards, ScopedTypeVariables, TypeOperators #-}
module GL.Setup
( Flag(..)
, Func(..)
, Factor(..)
, Shader(..)
, Setup
, enable
, disable
, setClearColour
, setDepthFunc
, setBlendFactors
, geometry
, buildProgram
, uniform
, runSetup
) where

import Control.Monad.Free.Freer
import Data.Functor.Union
import Effect.State
import GL.Array
import GL.Exception
import qualified GL.Geometry as Geometry
import GL.Program
import GL.Scalar
import qualified GL.Shader as Shader
import Graphics.GL.Core41
import Graphics.GL.Types
import qualified Linear.V4 as Linear

data Flag = DepthTest | Blending
data Func = Less | LessEqual | Always
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

data Setup a where
  Flag :: Flag -> Bool -> Setup ()
  SetDepthFunc :: Func -> Setup ()
  SetBlendFactors :: Factor -> Factor -> Setup ()
  SetClearColour :: Real n => Linear.V4 n -> Setup ()
  Geometry :: (Foldable v, GLScalar n) => [Geometry.Geometry (v n)] -> Setup (Geometry.GeometryArray n)
  BuildProgram :: [Shader] -> Setup GLProgram
  Uniform :: Shader.GLSLValue a => Setup (Shader.Var (Shader.Shader a))

enable :: InUnion fs Setup => Flag -> Eff fs ()
enable = send . (`Flag` True)

disable :: InUnion fs Setup => Flag -> Eff fs ()
disable = send . (`Flag` False)

setClearColour :: InUnion fs Setup => Linear.V4 Float -> Eff fs ()
setClearColour = send . SetClearColour

setDepthFunc :: InUnion fs Setup => Func -> Eff fs ()
setDepthFunc = send . SetDepthFunc

setBlendFactors :: InUnion fs Setup => Factor -> Factor -> Eff fs ()
setBlendFactors = (send .) . SetBlendFactors

geometry :: (Foldable v, GLScalar n, InUnion fs Setup) => [Geometry.Geometry (v n)] -> Eff fs (Geometry.GeometryArray n)
geometry = send . Geometry

buildProgram :: InUnion fs Setup => [Shader] -> Eff fs GLProgram
buildProgram = send . BuildProgram

uniform :: InUnion fs Setup => Shader.GLSLValue a => Eff fs (Shader.Var (Shader.Shader a))
uniform = send Uniform


runSetup :: InUnion fs IO => Eff (Setup ': fs) a -> Eff fs a
runSetup = fmap fst . flip runState 0 . iterFreerA runSetupAlgebra

data ArrayVertices a = ArrayVertices { arrayVertices :: [a], prevIndex :: Int, arrayRanges :: [Geometry.ArrayRange] }

runSetupAlgebra :: InUnion fs IO => forall a x. Union (Setup ': fs) x -> (x -> Eff (State Int ': fs) a) -> Eff (State Int ': fs) a
runSetupAlgebra union yield = case union of
  Here s -> case s of
    Flag f b -> do
      toggle b $ case f of
        DepthTest -> GL_DEPTH_TEST
        Blending -> GL_BLEND
      checkingGLError (yield ())
    SetDepthFunc f -> do
      glDepthFunc $ case f of
        Less -> GL_LESS
        LessEqual -> GL_LEQUAL
        Always -> GL_ALWAYS
      checkingGLError (yield ())
    SetBlendFactors source destination -> do
      glBlendFunc (factor source) (factor destination)
      checkingGLError (yield ())
    SetClearColour (Linear.V4 r g b a) -> do
      glClearColor (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
      checkingGLError (yield ())
    Geometry geometry -> do
      let vertices = foldr combineGeometry (ArrayVertices [] 0 []) geometry
      withVertices (arrayVertices vertices) (checkingGLError . yield . Geometry.GeometryArray (arrayRanges vertices))
    BuildProgram shaders -> withBuiltProgram (compileShader <$> shaders) (checkingGLError . yield)
    Uniform -> do
      name <- get
      put (succ name)
      yield (Shader.Uniform ('u' : show (name :: Int)))
  There t -> There t `Then` yield
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
        combineGeometry :: Geometry.Geometry (v n) -> ArrayVertices (v n) -> ArrayVertices (v n)
        combineGeometry (Geometry.Geometry mode vertices) ArrayVertices{..} =
          let count = length vertices
          in ArrayVertices
            (vertices ++ arrayVertices)
            (prevIndex + count)
            (Geometry.ArrayRange { mode = mode, firstVertexIndex = prevIndex, vertexCount = count } : arrayRanges)

compileShader :: Shader -> (GLenum, String)
compileShader (Vertex shader) = (GL_VERTEX_SHADER, Shader.toGLSL (Shader.elaborateVertexShader shader))
compileShader (Fragment shader) = (GL_FRAGMENT_SHADER, Shader.toGLSL (Shader.elaborateShader shader))
