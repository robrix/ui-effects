{-# LANGUAGE DataKinds, GADTs, RankNTypes #-}
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
import qualified Linear.V2 as Linear
import qualified Linear.V3 as Linear
import qualified Linear.V4 as Linear
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


toGLSL :: Shader k a -> String
toGLSL shader
  = pragma "version" "410"
  <> (uniforms shader >>= (<> "\n"))
  <> (inputs shader >>= (<> "\n"))
  <> (outputs shader >>= (<> "\n"))
  <> main (go shader)
  where go :: Shader k a -> String
        go (Set v value) = "  " <> set v <> " = " <> go value <> ";\n"
        go (Get v) = get v
        go (Lambda _ a) = go a
        go (V4 (Linear.V4 x y z w)) = "vec4(" <> intercalate ", " (show <$> [ x, y, z, w ]) <> ")"
        go (V3 (Linear.V3 x y z)) = "vec3(" <> intercalate ", " (show <$> [ x, y, z ]) <> ")"
        go (V2 (Linear.V2 x y)) = "vec2(" <> intercalate ", " (show <$> [ x, y ]) <> ")"
        go (Scalar x) = show x
        go (Add a b) = go a <> " + " <> go b
        go (Mul a b) = go a <> " * " <> go b
        go (Sub a b) = go a <> " - " <> go b
        go (Div a b) = go a <> " / " <> go b
        go _ = ""

        set :: Var 'Out k a -> String
        set Position = "gl_Position"
        set PointSize = "gl_PointSize"
        set Depth = "gl_FragDepth"
        set (Var s) = s
        set (Uniform s) = s

        get :: Var 'In k a -> String
        get Coord = "gl_FragCoord"
        get PointCoord = "gl_PointCoord"
        get FrontFacing = "gl_FrontFacing"
        get (Var s) = s
        get (Uniform s) = s

        inputs :: Shader k a -> [String]
        inputs (Set _ p) = inputs p
        inputs (Get (Var s)) = [ "in vec4 " <> s <> ";" ]
        inputs (Lambda _ a) = inputs a
        inputs _ = []

        outputs :: Shader k a -> [String]
        outputs (Set (Var s) c) = ("out vec4 " <> s <> ";") : outputs c
        outputs (Lambda _ a) = outputs a
        outputs _ = []

        uniforms :: Shader k a -> [String]
        uniforms (Set _ v) = uniforms v
        uniforms (Lambda _ a) = uniforms a
        uniforms (Get (Uniform s)) = [ "uniform vec4 " <> s <> ";" ]
        uniforms _ = []

        pragma k v = "#" <> k <> " " <> v <> "\n"
        main body = "void main(void) {\n" <> body <> "}"
