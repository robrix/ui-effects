{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, KindSignatures, RankNTypes, StandaloneDeriving #-}
module GL.Shader
( Var
, Shader
, ShaderF
, set
, get
, uniform
, bind
, v4
, (!*)
, position
, toGLSL
, GLShader(..)
, withCompiledShaders
) where

import Control.Exception
import Control.Monad.Free.Freer
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GL.Exception
import Graphics.GL.Core41
import Graphics.GL.Types
import qualified Linear.Matrix as Linear
import qualified Linear.V4 as Linear
import Prelude hiding (IO)

data Var a where
  Var :: String -> Var a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

data ShaderF a where
  -- Binding
  Uniform :: String -> ShaderF (Var a)
  Bind :: String -> ShaderF (Var a)

  -- Accessors
  Get :: Var a -> ShaderF a
  Set :: Foldable t => Var a -> t a -> ShaderF a

  -- Arithmetic
  Add, Sub, Mul, Div :: a -> a -> ShaderF a
  Abs, Signum :: a -> ShaderF a

  -- Matrix arithmetic
  MulMV :: (Foldable term, Foldable row, Foldable column) => term (row (column a)) -> term (column a) -> ShaderF (column a)

  -- Trigonometric
  Sin, Cos, Tan :: a -> ShaderF a
  ASin, ACos, ATan :: a -> ShaderF a
  SinH, CosH, TanH :: a -> ShaderF a
  ASinH, ACosH, ATanH :: a -> ShaderF a

  Exp, Log :: a -> ShaderF a

type Shader = Freer ShaderF


uniform :: String -> Shader (Var a)
uniform s = Freer (Free pure (Uniform s))

bind :: String -> Shader (Var a)
bind s = Freer (Free pure (Bind s))

get :: Var a -> Shader a
get v = Freer (Free pure (Get v))

set :: Var a -> Shader a -> Shader a
set var value = Freer (Free pure (Set var value))

v4 :: a -> a -> a -> a -> Shader (Linear.V4 a)
v4 x y z w = pure (Linear.V4 x y z w)

infixl 7 !*

(!*) :: Shader (Linear.M44 a) -> Shader (Linear.V4 a) -> Shader (Linear.V4 a)
matrix !* column = Freer (Free pure (MulMV matrix column))


-- Variables

position :: Var (Linear.V4 a)
position = Var "gl_Position"


-- Compilation

toGLSL :: Shader ShowS -> String
toGLSL = ($ "") . iterFreer toGLSLAlgebra

toGLSLAlgebra :: (x -> ShowS) -> ShaderF x -> ShowS
toGLSLAlgebra run shader = case shader of
  -- Uniform :: String -> ShaderF (Var a)
  -- Bind :: String -> ShaderF (Var a)

  Get (Var s) -> showString s
  -- Set :: Foldable t => Var a -> t a -> ShaderF a

  Add a b -> op '+' a b
  Sub a b -> op '-' a b
  Mul a b -> op '*' a b
  Div a b -> op '/' a b

  Abs a -> fun "abs" a
  Signum a -> fun "sign" a

  -- MulMV :: (Foldable term, Foldable row, Foldable column) => term (row (column a)) -> term (column a) -> ShaderF (column a)

  Sin a -> fun "sin" a
  Cos a -> fun "cos" a
  Tan a -> fun "tan" a
  ASin a -> fun "asin" a
  ACos a -> fun "acos" a
  ATan a -> fun "atan" a
  SinH a -> fun "sinh" a
  CosH a -> fun "cosh" a
  TanH a -> fun "tanh" a
  ASinH a -> fun "asinh" a
  ACosH a -> fun "acosh" a
  ATanH a -> fun "atanh" a

  Exp a -> fun "exp" a
  Log a -> fun "log" a

  where op o a b = showParen True $ run a . showChar ' ' . showChar o . showChar ' ' . run b
        fun f a = showString f . showParen True (run a)


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


-- Instances

instance Num a => Num (Shader a) where
  (+) = (wrap .) . Add
  (-) = (wrap .) . Sub
  (*) = (wrap .) . Mul

  abs = wrap . Abs
  signum = wrap . Signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (Shader a) where
  (/) = (wrap .) . Div
  fromRational = pure . fromRational

instance Floating a => Floating (Shader a) where
  sin = wrap . Sin
  cos = wrap . Cos
  tan = wrap . Tan
  asin = wrap . ASin
  acos = wrap . ACos
  atan = wrap . ATan
  sinh = wrap . SinH
  cosh = wrap . CosH
  tanh = wrap . TanH
  asinh = wrap . ASinH
  acosh = wrap . ACosH
  atanh = wrap . ATanH

  pi = pure pi
  exp = wrap . Exp
  log = wrap . Log

deriving instance Foldable ShaderF
