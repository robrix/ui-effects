{-# LANGUAGE DataKinds, GADTs, RankNTypes #-}
module GL.Shader where

import Control.Exception
import Data.List (intersperse)
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
    s <- checkShader source (GLShader shader)
    body s

withCompiledShaders :: [(GLenum, String)] -> ([GLShader] -> IO a) -> IO a
withCompiledShaders sources body = go sources []
  where go [] shaders = body shaders
        go ((t, source):xs) shaders = withCompiledShader t source (\ shader -> go xs (shader : shaders))

checkShader :: String -> GLShader -> IO GLShader
checkShader source = fmap GLShader . checkStatus glGetShaderiv glGetShaderInfoLog (Source source) GL_COMPILE_STATUS . unGLShader


toGLSL :: Shader k a -> String
toGLSL shader
  = pragma "version" "410"
  . foldr (.) id ((. showString "\n") <$> uniforms shader)
  . foldr (.) id ((. showString "\n") <$> inputs shader)
  . foldr (.) id ((. showString "\n") <$> outputs shader)
  . main (go shader) $ ""
  where go :: Shader k a -> ShowS
        go (Set v value) = showString "  " . showString (set v) . showString " = " . go value . showString ";\n"
        go (Get v) = showString $ get v
        go (Lambda _ a) = go a
        go (V4 (Linear.V4 x y z w)) = vector [ x, y, z, w ]
        go (V3 (Linear.V3 x y z)) = vector [ x, y, z ]
        go (V2 (Linear.V2 x y)) = vector [ x, y ]
        go (Scalar x) = shows x
        go (Add a b) = showParen True $ go a . showString " + " . go b
        go (Mul a b) = showParen True $ go a . showString " * " . go b
        go (Sub a b) = showParen True $ go a . showString " - " . go b
        go (Div a b) = showParen True $ go a . showString " / " . go b
        go _ = id

        vector vs = showString "vec" . shows (length vs) . showParen True (foldr (.) id (intersperse (showString ", ") (shows <$> vs)))

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

        inputs :: Shader k a -> [ShowS]
        inputs (Set _ p) = inputs p
        inputs (Get (Var s)) = [ showString $ "in vec4 " <> s <> ";" ]
        inputs (Lambda _ a) = inputs a
        inputs (Add a b) = inputs a <> inputs b
        inputs (Mul a b) = inputs a <> inputs b
        inputs _ = []

        outputs :: Shader k a -> [ShowS]
        outputs (Set (Var s) c) = showString ("out vec4 " <> s <> ";") : outputs c
        outputs (Lambda _ a) = outputs a
        outputs (Add a b) = outputs a <> outputs b
        outputs (Mul a b) = outputs a <> outputs b
        outputs _ = []

        uniforms :: Shader k a -> [ShowS]
        uniforms (Set _ v) = uniforms v
        uniforms (Lambda _ a) = uniforms a
        uniforms (Get (Uniform s)) = [ showString $ "uniform vec4 " <> s <> ";" ]
        uniforms (Add a b) = uniforms a <> uniforms b
        uniforms (Mul a b) = uniforms a <> uniforms b
        uniforms _ = []

        pragma k v = showString $ "#" <> k <> " " <> v <> "\n"
        main body = showString "void main(void) {\n" . body . showString "}"
