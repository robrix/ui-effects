{-# LANGUAGE ScopedTypeVariables #-}
module GL.Array where

import Data.Foldable (for_, toList)
import Data.List (uncons)
import Data.Proxy
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GL.Scalar
import Graphics.GL.Core41
import Graphics.GL.Types

newtype GLArray n = GLArray { unGLArray :: GLuint }

withVertices :: forall v n a. (Foldable v, GLScalar n) => [v n] -> (GLArray n -> IO a) -> IO a
withVertices vertices body = alloca $ \ p -> do
  glGenBuffers 1 p
  vbo <- peek p
  let vertexCount = length vertices
  let fieldCount = maybe 1 (length . fst) (uncons vertices)
  let fieldSize = sizeOf (0 :: n)
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
  glVertexAttribPointer 0 (fromIntegral fieldCount) (glType (Proxy :: Proxy n)) GL_FALSE 0 nullPtr
  body $ GLArray array
