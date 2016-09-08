module GL.Shader.Fragment where

import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL.Core41
import Graphics.Shader.Fragment

compile :: Show a => a -> IO ()
compile fragment = do
  shader <- glCreateShader GL_FRAGMENT_SHADER
  withCString (show fragment) $ \ source -> do
    alloca $ \ p -> do
      poke p source
      glShaderSource shader 1 p nullPtr
  glCompileShader shader
