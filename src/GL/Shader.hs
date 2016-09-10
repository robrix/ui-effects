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

newtype Shader = Shader { unShader :: GLuint }

withShader :: GLenum -> (Shader -> IO a) -> IO a
withShader shaderType = bracket
  (Shader <$> glCreateShader shaderType)
  (glDeleteShader . unShader)

withCompiledShader :: GLenum -> String -> (Shader -> IO a) -> IO a
withCompiledShader shaderType source body = withShader shaderType $ \ (Shader shader) -> do
    withCString source $ \ source ->
      alloca $ \ p -> do
        poke p source
        glShaderSource shader 1 p nullPtr
    glCompileShader shader
    s <- checkShader (Shader shader)
    body s

withCompiledShaders :: [(GLenum, String)] -> ([Shader] -> IO a) -> IO a
withCompiledShaders sources body = go sources []
  where go [] shaders = body shaders
        go ((t, source):xs) shaders = withCompiledShader t source (\ shader -> go xs (shader : shaders))

checkShader :: Shader -> IO Shader
checkShader = fmap Shader . checkStatus glGetShaderiv glGetShaderInfoLog GL_COMPILE_STATUS . unShader
