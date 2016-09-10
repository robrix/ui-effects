{-# LANGUAGE GADTs, MultiParamTypeClasses, RankNTypes #-}
module GL.Shader.Fragment where

import Control.Exception
import Control.Monad
import Data.Foldable (for_)
import Data.List (intercalate)
import Data.Monoid
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GL.Exception
import GL.Shader
import Graphics.GL.Core41
import Graphics.GL.Types
import Graphics.Shader.Fragment
import Linear.V3
import Prelude hiding (IO)

newtype Program = Program { unProgram :: GLuint }

newtype VAO = VAO { unVAO :: GLuint }

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


withVertices :: [V3 Float] -> (VAO -> IO a) -> IO a
withVertices vertices body = alloca $ \ p -> do
  glGenBuffers 1 p
  vbo <- peek p
  let bytes = length vertices * 3 * sizeOf (0 :: Float)
  allocaBytes bytes $ \ p -> do
    for_ (zip [0..] vertices) (uncurry (pokeElemOff p))
    glBindBuffer GL_ARRAY_BUFFER vbo
    glBufferData GL_ARRAY_BUFFER (fromIntegral bytes) (castPtr p) GL_STATIC_DRAW
  glGenVertexArrays 1 p
  vao <- peek p
  glBindVertexArray vao
  glEnableVertexAttribArray 0
  glBindBuffer GL_ARRAY_BUFFER vbo
  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr
  body $ VAO vao

withProgram :: (Program -> IO a) -> IO a
withProgram = bracket
  (Program <$> glCreateProgram)
  (glDeleteProgram . unProgram)

withLinkedProgram :: [Shader] -> (Program -> IO a) -> IO a
withLinkedProgram shaders body = withProgram $ \ (Program program) -> do
  for_ shaders (glAttachShader program . unShader)
  glLinkProgram program
  for_ shaders (glDetachShader program . unShader)
  p <- checkProgram (Program program)
  body p


withBuiltProgram :: [(GLenum, String)] -> (Program -> IO a) -> IO a
withBuiltProgram sources body = withCompiledShaders sources (`withLinkedProgram` body)


checkProgram :: Program -> IO Program
checkProgram = fmap Program . checkStatus glGetProgramiv glGetProgramInfoLog GL_LINK_STATUS . unProgram
