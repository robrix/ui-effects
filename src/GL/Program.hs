{-# LANGUAGE FlexibleInstances, RankNTypes #-}
module GL.Program where

import Control.Exception (bracket)
import Data.Foldable (for_)
import Foreign.C.String
import GL.Exception
import GL.Shader
import Graphics.GL.Core41
import Graphics.GL.Types
import qualified Linear.V4 as Linear
import Prelude hiding (IO)

newtype GLProgram = GLProgram { unGLProgram :: GLuint }
  deriving Show

newtype GLUniform a = GLUniform { unGLUniform :: GLint }

withProgram :: (GLProgram -> IO a) -> IO a
withProgram = bracket
  (GLProgram <$> glCreateProgram)
  (glDeleteProgram . unGLProgram)

withLinkedProgram :: [GLShader] -> (GLProgram -> IO a) -> IO a
withLinkedProgram shaders body = withProgram $ \ (GLProgram program) -> do
  for_ shaders (glAttachShader program . unGLShader)
  glLinkProgram program
  for_ shaders (glDetachShader program . unGLShader)
  p <- checkProgram (GLProgram program)
  body p


withBuiltProgram :: [(GLenum, String)] -> (GLProgram -> IO a) -> IO a
withBuiltProgram sources body = withCompiledShaders sources (`withLinkedProgram` body)


checkProgram :: GLProgram -> IO GLProgram
checkProgram = fmap GLProgram . checkStatus glGetProgramiv glGetProgramInfoLog Other GL_LINK_STATUS . unGLProgram


class GLProgramUniform t where
  setUniformValue :: GLProgram -> String -> t -> IO ()

instance GLProgramUniform (Linear.V4 Float) where
  setUniformValue program name (Linear.V4 x y z w)= do
    location <- withCString name (glGetUniformLocation (unGLProgram program))
    glProgramUniform4f (unGLProgram program) location x y z w
    checkGLError
