{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes #-}
module GL.Program where

import Data.Foldable (for_, toList)
import Data.Functor.Union
import Foreign.Ptr
import GL.Exception
import GL.Shader
import Graphics.GL.Core41
import Graphics.GL.Types
import qualified Linear.V4 as Linear
import qualified Linear.Matrix as Linear

newtype GLProgram = GLProgram { unGLProgram :: GLuint }
  deriving Show

newtype GLUniform a = GLUniform { unGLUniform :: GLint }

withProgram :: InUnion fs IO => (GLProgram -> Eff fs a) -> Eff fs a
withProgram = bracket
  (GLProgram <$> glCreateProgram)
  (glDeleteProgram . unGLProgram)

withLinkedProgram :: InUnion fs IO => [GLShader] -> (GLProgram -> Eff fs a) -> Eff fs a
withLinkedProgram shaders body = withProgram $ \ (GLProgram program) -> do
  for_ shaders (glAttachShader program . unGLShader)
  glLinkProgram program
  for_ shaders (glDetachShader program . unGLShader)
  p <- checkProgram (GLProgram program)
  body p


withBuiltProgram :: InUnion fs IO => [(GLenum, String)] -> (GLProgram -> Eff fs a) -> Eff fs a
withBuiltProgram sources body = withCompiledShaders sources (`withLinkedProgram` body)


checkProgram :: InUnion fs IO => GLProgram -> Eff fs GLProgram
checkProgram = fmap GLProgram . checkStatus glGetProgramiv glGetProgramInfoLog Other GL_LINK_STATUS . unGLProgram


class GLProgramUniform t where
  setUniformValue :: InUnion fs IO => GLProgram -> Var (Shader t) -> t -> Eff fs ()

instance GLProgramUniform (Linear.V4 Float) where
  setUniformValue program var (Linear.V4 x y z w)= do
    location <- withCString (varName var) (glGetUniformLocation (unGLProgram program))
    glProgramUniform4f (unGLProgram program) location x y z w
    checkGLError

instance GLProgramUniform (Linear.V4 Double) where
  setUniformValue program var (Linear.V4 x y z w)= do
    location <- withCString (varName var) (glGetUniformLocation (unGLProgram program))
    glProgramUniform4d (unGLProgram program) location x y z w
    checkGLError

instance GLProgramUniform (Linear.M44 Float) where
  setUniformValue program var matrix = do
    location <- withCString (varName var) (glGetUniformLocation (unGLProgram program))
    let fieldCount = sum (length <$> matrix)
    let fieldSize = sizeOf (0 :: Float)
    let byteCount = fieldCount * fieldSize
    allocaBytes byteCount $ \ p -> do
      for_ (zip [0..] (toList (Linear.transpose matrix) >>= toList)) (uncurry (pokeElemOff p))
      glProgramUniformMatrix4fv (unGLProgram program) location 1 GL_FALSE (castPtr p)
    checkGLError
