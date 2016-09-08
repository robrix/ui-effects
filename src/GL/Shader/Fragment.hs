{-# LANGUAGE RecordWildCards #-}
module GL.Shader.Fragment where

import Control.Exception
import Control.Monad
import Data.Typeable
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL.Core41
import Graphics.GL.Types
import Graphics.Shader.Fragment

newtype Shader = Shader { unShader :: GLuint }

newtype ShaderException = ShaderException String
  deriving (Show, Typeable)


toGLSL :: Fragment () -> String
toGLSL _ = ""


compile :: Show a => a -> IO Shader
compile fragment = do
  shader <- glCreateShader GL_FRAGMENT_SHADER
  withCString (show fragment) $ \ source -> do
    alloca $ \ p -> do
      poke p source
      glShaderSource shader 1 p nullPtr
  glCompileShader shader
  checkShader (Shader shader)


checkShader :: Shader -> IO Shader
checkShader Shader{..} = do
  success <- alloca $ \ p -> do
    glGetShaderiv unShader GL_COMPILE_STATUS p
    peek p
  when (success == GL_FALSE) $ do
    l <- alloca $ \ p -> do
      glGetShaderiv unShader GL_INFO_LOG_LENGTH p
      peek p
    log <- allocaBytes (fromIntegral l) $ \ bytes -> do
      glGetShaderInfoLog unShader l nullPtr bytes
      peekCString bytes
    throw $ ShaderException log
  pure (Shader unShader)

instance Exception ShaderException
