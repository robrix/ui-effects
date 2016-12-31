{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, RankNTypes, RecordWildCards, ScopedTypeVariables, TypeOperators #-}
module GL.Setup
( Flag(..)
, Func(..)
, Factor(..)
, Shader(..)
, SetupF
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

import Control.Monad.Effect
import Control.Monad.Effect.Internal
import Control.Monad.Effect.State
import Control.Monad.Free.Freer
import Control.Monad.IO.Class
import Data.Foldable (toList)
import Data.Functor.Classes
import GL.Array
import GL.Exception
import qualified GL.Geometry as Geometry
import GL.Program
import GL.Scalar
import qualified GL.Shader as Shader
import Graphics.GL.Core41
import Graphics.GL.Types
import qualified Linear.V4 as Linear
import Prelude hiding (IO)
import qualified Prelude
import Text.Show (showListWith)

data Flag = DepthTest | Blending
  deriving Show
data Func = Less | LessEqual | Always
  deriving Show
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
  deriving Show

data Shader where
  Vertex :: Shader.Shader Shader.Vertex -> Shader
  Fragment :: Shader.GLSLValue a => Shader.Shader a -> Shader

data SetupF a where
  Flag :: Flag -> Bool -> SetupF ()
  SetDepthFunc :: Func -> SetupF ()
  SetBlendFactors :: Factor -> Factor -> SetupF ()
  SetClearColour :: Real n => Linear.V4 n -> SetupF ()
  Geometry :: (Foldable v, GLScalar n) => [Geometry.Geometry (v n)] -> SetupF (Geometry.GeometryArray n)
  BuildProgram :: [Shader] -> SetupF GLProgram
  RunIO :: IO a -> SetupF a
  Uniform :: Shader.GLSLValue a => SetupF (Shader.Var (Shader.Shader a))

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

geometry ::  (Foldable v, GLScalar n) => [Geometry.Geometry (v n)] -> Setup (Geometry.GeometryArray n)
geometry = liftF . Geometry

buildProgram :: [Shader] -> Setup GLProgram
buildProgram = liftF . BuildProgram

uniform :: Shader.GLSLValue a => Setup (Shader.Var (Shader.Shader a))
uniform = liftF Uniform


runSetup :: Setup a -> Prelude.IO a
runSetup = runSetupEffects . iterFreerA runSetupAlgebra

runSetupEffects :: Eff '[State Int, Prelude.IO] a -> Prelude.IO a
runSetupEffects = runM . fmap fst . flip runState 0

data ArrayVertices a = ArrayVertices { arrayVertices :: [a], nextIndex :: Int, arrayRanges :: [Geometry.ArrayRange] }
  deriving Show

runSetupAlgebra :: forall a x. (x -> Eff '[State Int, Prelude.IO] a) -> SetupF x -> Eff '[State Int, Prelude.IO] a
runSetupAlgebra run s = case s of
  Flag f b -> do
    sendIO $ toggle b $ case f of
      DepthTest -> GL_DEPTH_TEST
      Blending -> GL_BLEND
    send $ checkingGLError (runSetupEffects (run ()))
  SetDepthFunc f -> do
    sendIO $ glDepthFunc $ case f of
      Less -> GL_LESS
      LessEqual -> GL_LEQUAL
      Always -> GL_ALWAYS
    send $ checkingGLError (runSetupEffects (run ()))
  SetBlendFactors source destination -> do
    sendIO (glBlendFunc (factor source) (factor destination))
    send $ checkingGLError (runSetupEffects (run ()))
  SetClearColour (Linear.V4 r g b a) -> do
    sendIO (glClearColor (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a))
    send $ checkingGLError (runSetupEffects (run ()))
  Geometry geometry -> send $ do
    let vertices = foldr combineGeometry (ArrayVertices [] 0 []) geometry
    withVertices (arrayVertices vertices) (checkingGLError . runSetupEffects . run . Geometry.GeometryArray (arrayRanges vertices))
  BuildProgram shaders -> send $ withBuiltProgram (compileShader <$> shaders) (checkingGLError . runSetupEffects . run)
  RunIO io -> send io >>= run
  Uniform -> do
    name <- get
    put (succ name)
    run (Shader.Uniform ('u' : show (name :: Int)))
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
        sendIO io = send (io :: Prelude.IO ())

combineGeometry :: Geometry.Geometry (v n) -> ArrayVertices (v n) -> ArrayVertices (v n)
combineGeometry (Geometry.Geometry mode vertices) ArrayVertices{..} =
  let count = length vertices
  in ArrayVertices
    (vertices ++ arrayVertices)
    (nextIndex + count)
    (Geometry.ArrayRange { mode = mode, firstVertexIndex = nextIndex, vertexCount = count } : arrayRanges)

compileShader :: Shader -> (GLenum, String)
compileShader (Vertex shader) = (GL_VERTEX_SHADER, Shader.toGLSL (Shader.elaborateVertexShader shader))
compileShader (Fragment shader) = (GL_FRAGMENT_SHADER, Shader.toGLSL (Shader.elaborateShader shader))


-- Instances

instance MonadIO Setup where
  liftIO = liftF . RunIO

instance Show Shader where
  showsPrec d s = case s of
    Vertex shader -> showsUnaryWith showsPrec "Vertex" d shader
    Fragment shader -> showsUnaryWith (liftShowsPrec (const Shader.showsGLSLValue) (showListWith Shader.showsGLSLValue)) "Fragment" d shader

instance Show1 SetupF where
  liftShowsPrec _ _ d s = case s of
    Flag flag enabled -> showsBinaryWith showsPrec showsPrec "Flag" d flag enabled
    SetDepthFunc func -> showsUnaryWith showsPrec "SetDepthFunc" d func
    SetBlendFactors source destination -> showsBinaryWith showsPrec showsPrec "SetBlendFactors" d source destination
    SetClearColour colour -> showsUnaryWith showsPrec "SetClearColour" d (toFloat <$> colour)
    Geometry meshes -> showsUnaryWith showsPrec "Geometry" d (toFloatGeometry <$> meshes)
    BuildProgram shaders -> showsUnaryWith showsPrec "BuildProgram" d shaders
    RunIO _ -> showString "RunIO _"
    Uniform -> showString "Uniform"
    where toFloat :: Real a => a -> Float
          toFloat = (fromRational :: Rational -> Float) . toRational
          toFloatGeometry :: Geometry.Geometry (v n) -> Geometry.Geometry [Float]
          toFloatGeometry (Geometry.Geometry mode vertices) = Geometry.Geometry mode (fmap toFloat . toList <$> vertices)
