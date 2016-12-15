{-# LANGUAGE DefaultSignatures, GADTs, ScopedTypeVariables #-}
module GL.Shader.Core where

import Control.Monad.Free.Freer
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Proxy
import qualified Linear.V4 as Linear

data ExprF a where
  V4 :: GLSLValue a => Linear.V4 a -> ExprF (Linear.V4 a)

  Add :: a -> a -> ExprF a
  Sub :: a -> a -> ExprF a
  Mul :: a -> a -> ExprF a
  Div :: a -> a -> ExprF a
  Abs :: a -> ExprF a
  Signum :: a -> ExprF a

  Sin :: a -> ExprF a
  Cos :: a -> ExprF a
  Tan :: a -> ExprF a
  ASin :: a -> ExprF a
  ACos :: a -> ExprF a
  ATan :: a -> ExprF a
  SinH :: a -> ExprF a
  CosH :: a -> ExprF a
  TanH :: a -> ExprF a
  ASinH :: a -> ExprF a
  ACosH :: a -> ExprF a
  ATanH :: a -> ExprF a

  Exp :: a -> ExprF a
  Log :: a -> ExprF a

type Expr = Freer ExprF

data DeclF a where
  In :: GLSLValue a => String -> DeclF a
  Out :: GLSLValue a => String -> DeclF a
  Uniform :: GLSLValue a => String -> DeclF a


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