{-# LANGUAGE FlexibleInstances, GADTs, RankNTypes, StandaloneDeriving #-}
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
import Data.Functor.Classes
import Data.Proxy
import Data.Typeable
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
  Var :: Typeable a => String -> Var a
  deriving Typeable

data ShaderF a where
  -- Binding
  Uniform :: String -> ShaderF (Var (Shader a))
  Bind :: String -> ShaderF (Var (Shader a))

  -- Accessors
  Get :: Var (Shader a) -> ShaderF a
  Set :: Var a -> a -> ShaderF a

  -- Arithmetic
  Add :: a -> a -> ShaderF a
  Sub :: a -> a -> ShaderF a
  Mul :: a -> a -> ShaderF a
  Div :: a -> a -> ShaderF a
  Abs :: a -> ShaderF a
  Signum :: a -> ShaderF a

  -- Matrix arithmetic
  MulMV :: Shader (Linear.V4 a) -> Shader a -> ShaderF a

  -- Trigonometric
  Sin :: a -> ShaderF a
  Cos :: a -> ShaderF a
  Tan :: a -> ShaderF a
  ASin :: a -> ShaderF a
  ACos :: a -> ShaderF a
  ATan :: a -> ShaderF a
  SinH :: a -> ShaderF a
  CosH :: a -> ShaderF a
  TanH :: a -> ShaderF a
  ASinH :: a -> ShaderF a
  ACosH :: a -> ShaderF a
  ATanH :: a -> ShaderF a

  Exp :: a -> ShaderF a
  Log :: a -> ShaderF a
  deriving Typeable

type Shader = Freer ShaderF


uniform :: String -> Shader (Var (Shader a))
uniform = liftF . Uniform

bind :: String -> Shader (Var (Shader a))
bind = liftF . Bind

get :: Var (Shader a) -> Shader a
get = liftF . Get

set :: Var (Shader a) -> Shader a -> Shader a
set var value = wrap (Set var value)

v4 :: a -> a -> a -> a -> Shader (Linear.V4 a)
v4 x y z w = pure (Linear.V4 x y z w)

infixl 7 !*

(!*) :: Shader (Linear.M44 a) -> Shader (Linear.V4 a) -> Shader (Linear.V4 a)
matrix !* column = Freer (Free pure (MulMV matrix column))


-- Variables

position :: Var (Linear.V4 Float)
position = Var "gl_Position"


-- Compilation

toGLSL :: Shader ShowS -> String
toGLSL = ($ "") . (showString "#version 410\n" .) . iterFreer toGLSLAlgebra

toGLSLAlgebra :: (x -> ShowS) -> ShaderF x -> ShowS
toGLSLAlgebra run shader = case shader of
  Uniform s -> showString "uniform" . sp . showString s . showChar ';'
  Bind s -> showString "out" . sp . showString s . showChar ';'

  Get v -> var v
  Set v value -> var v . sp . showChar '=' . sp . run value . showChar ';'

  Add a b -> op '+' a b
  Sub a b -> op '-' a b
  Mul a b -> op '*' a b
  Div a b -> op '/' a b

  Abs a -> fun "abs" a
  Signum a -> fun "sign" a

  MulMV matrix column -> recur vec matrix . showChar '*' . recur run column

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

  where op o a b = showParen True $ run a . sp . showChar o . sp . run b
        fun f a = showString f . showParen True (run a)
        var (Var s) = showString s
        sp = showChar ' '
        vec v = showString "vec" . shows (length v) . showParen True (foldr (.) id (run <$> v))
        recur = (iterFreer toGLSLAlgebra .) . fmap


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


-- Classes

class GLSLType t where
  showsGLSLType :: Proxy t -> ShowS


-- Instances

deriving instance Eq (Var a)
deriving instance Foldable Var
deriving instance Ord (Var a)
deriving instance Show (Var a)

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

instance Show1 ShaderF where
  liftShowsPrec sp sl d shader = case shader of
    Uniform s -> showsUnaryWith showsPrec "Uniform" d s
    Bind s -> showsUnaryWith showsPrec "Bind" d s

    Get v -> showsUnaryWith showsPrec "Get" d v
    Set v value -> showsBinaryWith showsPrec sp "Set" d v value

    Add a b -> showsBinaryWith sp sp "Add" d a b
    Sub a b -> showsBinaryWith sp sp "Sub" d a b
    Mul a b -> showsBinaryWith sp sp "Mul" d a b
    Div a b -> showsBinaryWith sp sp "Div" d a b

    Abs a -> showsUnaryWith sp "Abs" d a
    Signum a -> showsUnaryWith sp "Signum" d a

    MulMV a b -> showsBinaryWith (liftShowsPrec (liftShowsPrec sp sl) (liftShowList sp sl)) (liftShowsPrec sp sl) "MulMV" d a b

    Sin a -> showsUnaryWith sp "Sin" d a
    Cos a -> showsUnaryWith sp "Cos" d a
    Tan a -> showsUnaryWith sp "Tan" d a
    ASin a -> showsUnaryWith sp "ASin" d a
    ACos a -> showsUnaryWith sp "ACos" d a
    ATan a -> showsUnaryWith sp "ATan" d a
    SinH a -> showsUnaryWith sp "SinH" d a
    CosH a -> showsUnaryWith sp "CosH" d a
    TanH a -> showsUnaryWith sp "TanH" d a
    ASinH a -> showsUnaryWith sp "ASinH" d a
    ACosH a -> showsUnaryWith sp "ACosH" d a
    ATanH a -> showsUnaryWith sp "ATanH" d a

    Exp a -> showsUnaryWith sp "Exp" d a
    Log a -> showsUnaryWith sp "Log" d a


instance GLSLType Float where
  showsGLSLType _ = showString "float"

instance GLSLType Bool where
  showsGLSLType _ = showString "bool"

instance GLSLType (Linear.V4 Float) where
  showsGLSLType _ = showString "vec4"

instance GLSLType (Linear.V4 Bool) where
  showsGLSLType _ = showString "bvec4"

instance GLSLType (Linear.M44 Float) where
  showsGLSLType _ = showString "mat4"
