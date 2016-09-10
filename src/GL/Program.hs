{-# LANGUAGE RankNTypes #-}
module GL.Program where

import Control.Exception (bracket)
import Data.Foldable (for_)
import GL.Exception
import GL.Shader
import Graphics.GL.Core41
import Graphics.GL.Types
import Prelude hiding (IO)

newtype GLProgram = GLProgram { unGLProgram :: GLuint }

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
checkProgram = fmap GLProgram . checkStatus glGetProgramiv glGetProgramInfoLog GL_LINK_STATUS . unGLProgram
