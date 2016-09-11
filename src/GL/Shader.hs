{-# LANGUAGE GADTs, RankNTypes #-}
module GL.Shader where

import Control.Exception
import Data.List (intercalate)
import Data.Monoid ((<>))
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GL.Exception
import Graphics.GL.Core41
import Graphics.GL.Types
import Graphics.Shader
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


toGLSL :: Shader () -> String
toGLSL shader
  = pragma "version" "410"
  <> "out vec4 fragColour;\n"
  <> main (go shader)
  where go :: Shader a -> String
        go (SetColour c) = "  fragColour = " <> go c <> ";\n"
        go (SetPosition v) = "  gl_Position = " <> go v <> ";\n"
        go (V4 x y z w) = "vec4(" <> intercalate ", " (show <$> [ x, y, z, w ]) <> ")"
        go (V2 x y) = "vec2(" <> intercalate ", " (show <$> [ x, y ]) <> ")"
        go (Add a b) = go a <> " + " <> go b
        go (Mul a b) = go a <> " * " <> go b
        go (Sub a b) = go a <> " - " <> go b
        go (Div a b) = go a <> " / " <> go b
        go _ = ""
        pragma k v = "#" <> k <> " " <> v <> "\n"
        main body = "void main(void) {\n" <> body <> "}"
