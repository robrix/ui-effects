{-# LANGUAGE RankNTypes #-}
module GL.Shader where

import Control.Exception
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GL.Exception
import Graphics.GL.Core41
import Graphics.GL.Types
import Prelude hiding (IO)

newtype GLShader = GLShader { unGLShader :: GLuint }

withShader :: GLenum -> (GLShader -> IO a) -> IO a
withShader shaderType = bracket
  (GLShader <$> glCreateShader shaderType)
  (glDeleteShader . unGLShader)

withCompiledShader :: GLenum -> String -> (GLShader -> IO a) -> IO a
withCompiledShader shaderType source body = withShader shaderType $ \ (GLShader shader) -> do
    withCString source $ \ source ->
      alloca $ \ p -> do
        poke p source
        glShaderSource shader 1 p nullPtr
    glCompileShader shader
    s <- checkShader (GLShader shader)
    body s

withCompiledShaders :: [(GLenum, String)] -> ([GLShader] -> IO a) -> IO a
withCompiledShaders sources body = go sources []
  where go [] shaders = body shaders
        go ((t, source):xs) shaders = withCompiledShader t source (\ shader -> go xs (shader : shaders))

checkShader :: GLShader -> IO GLShader
checkShader = fmap GLShader . checkStatus glGetShaderiv glGetShaderInfoLog GL_COMPILE_STATUS . unGLShader
