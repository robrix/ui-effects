{-# LANGUAGE GADTs, MultiParamTypeClasses, RankNTypes #-}
module GL.Shader.Fragment where

import Control.Exception
import Control.Monad
import Data.Foldable (for_, toList)
import Data.List (intercalate, uncons)
import Data.Monoid
import Data.Proxy
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GL.Exception
import GL.Shader
import Graphics.GL.Core41
import Graphics.GL.Types
import Graphics.Shader.Fragment
import Prelude hiding (IO)

newtype Program = Program { unProgram :: GLuint }

newtype GLArray n = GLArray { unGLArray :: GLuint }

class Num n => GLScalar n where
  glType :: Proxy n -> GLenum

instance GLScalar Float where
  glType _ = GL_FLOAT

toGLSL :: Fragment () -> String
toGLSL shader
  = pragma "version" "410"
  <> "out vec4 fragColour;\n"
  <> main (go shader)
  where go :: Fragment a -> String
        go (SetColour c) = "  fragColour = " <> go c <> ";\n"
        go (V4 x y z w) = "vec4(" <> intercalate ", " (show <$> [ x, y, z, w ]) <> ")"
        go (V2 x y) = "vec2(" <> intercalate ", " (show <$> [ x, y ]) <> ")"
        go (Add a b) = go a <> " + " <> go b
        go (Mul a b) = go a <> " * " <> go b
        go (Sub a b) = go a <> " - " <> go b
        go (Div a b) = go a <> " / " <> go b
        go _ = ""
        pragma k v = "#" <> k <> " " <> v <> "\n"
        main body = "void main(void) {\n" <> body <> "}"


withVertices :: Foldable v => [v Float] -> (GLArray Float -> IO a) -> IO a
withVertices vertices body = alloca $ \ p -> do
  glGenBuffers 1 p
  vbo <- peek p
  let vertexCount = length vertices
  let fieldCount = maybe 1 (length . fst) (uncons vertices)
  let fieldSize = sizeOf (0 :: Float)
  let byteCount = vertexCount * fieldCount * fieldSize
  allocaBytes byteCount $ \ p -> do
    for_ (zip [0..] (vertices >>= toList)) (uncurry (pokeElemOff p))
    glBindBuffer GL_ARRAY_BUFFER vbo
    glBufferData GL_ARRAY_BUFFER (fromIntegral byteCount) (castPtr p) GL_STATIC_DRAW
  glGenVertexArrays 1 p
  array <- peek p
  glBindVertexArray array
  glEnableVertexAttribArray 0
  glBindBuffer GL_ARRAY_BUFFER vbo
  glVertexAttribPointer 0 (fromIntegral fieldCount) GL_FLOAT GL_FALSE 0 nullPtr
  body $ GLArray array

withProgram :: (Program -> IO a) -> IO a
withProgram = bracket
  (Program <$> glCreateProgram)
  (glDeleteProgram . unProgram)

withLinkedProgram :: [GLShader] -> (Program -> IO a) -> IO a
withLinkedProgram shaders body = withProgram $ \ (Program program) -> do
  for_ shaders (glAttachShader program . unGLShader)
  glLinkProgram program
  for_ shaders (glDetachShader program . unGLShader)
  p <- checkProgram (Program program)
  body p


withBuiltProgram :: [(GLenum, String)] -> (Program -> IO a) -> IO a
withBuiltProgram sources body = withCompiledShaders sources (`withLinkedProgram` body)


checkProgram :: Program -> IO Program
checkProgram = fmap Program . checkStatus glGetProgramiv glGetProgramInfoLog GL_LINK_STATUS . unProgram
