{-# LANGUAGE DefaultSignatures, GADTs, ScopedTypeVariables #-}
module GL.Shader.Core where

import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Proxy
import qualified Linear.V4 as Linear

-- Classes

class GLSLValue v where
  showsGLSLType :: Proxy v -> ShowS
  showsGLSLVecType :: Proxy v -> ShowS
  showsGLSLValue :: v -> ShowS
  default showsGLSLValue :: Show v => v -> ShowS
  showsGLSLValue = shows


-- Instances

instance GLSLValue () where
  showsGLSLType _ = showString "void"
  showsGLSLVecType _ = showString "void"
  showsGLSLValue = const id

instance GLSLValue Float where
  showsGLSLType _ = showString "float"
  showsGLSLVecType _ = showString "vec4"

instance GLSLValue Bool where
  showsGLSLType _ = showString "bool"
  showsGLSLVecType _ = showString "bvec4"
  showsGLSLValue v = showString $ if v then "true" else "false"

instance GLSLValue a => GLSLValue (Linear.V4 a) where
  showsGLSLType _ = showsGLSLVecType (Proxy :: Proxy a)
  showsGLSLVecType _ = showString "mat4"
  showsGLSLValue v = showsGLSLVecType (Proxy :: Proxy a) . showParen True (foldr (.) id (intersperse (showString ", ") (showsGLSLValue <$> toList v)))
