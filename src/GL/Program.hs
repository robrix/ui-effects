{-# LANGUAGE RankNTypes #-}
module GL.Program where

import Control.Exception (bracket)
import Data.Foldable (for_)
import Foreign.C.String
import GL.Exception
import GL.Shader
import Graphics.GL.Core41
import Graphics.GL.Types
import Linear.V4
import Prelude hiding (IO)

newtype GLProgram = GLProgram { unGLProgram :: GLuint }

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


getUniform :: GLProgram -> String -> IO (Maybe (GLUniform a))
getUniform program name = do
  location <- withCString name (glGetUniformLocation (unGLProgram program))
  pure  $! if location == negate 1
    then Nothing
    else Just (GLUniform location)

setUniformValue :: GLProgram -> GLUniform (V4 Float) -> V4 Float -> IO ()
setUniformValue program uniform (V4 x y z w) = glProgramUniform4f (unGLProgram program) (unGLUniform uniform) x y z w
