{-# LANGUAGE DefaultSignatures, FlexibleInstances, GADTs, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeFamilies #-}
module GL.Shader
( Var
, Shader
, ShaderF
, Vertex(position, pointSize, clipDistance)
, vertex
, set
, get
, uniform
, input
, output
, function
, v4
, (!*)
, elaborateVertexShader
, elaborateShader
, toGLSL
, GLShader(..)
, withCompiledShaders
, GLSLValue(..)
, IsShader
, toShader
) where

import Control.Exception
import Control.Monad
import Control.Monad.Free.Freer
import Data.Foldable (toList)
import Data.Functor.Classes
import Data.List (intersperse)
import Data.Proxy
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
  In :: GLSLValue a => String -> Var a
  Out :: GLSLValue a => String -> Var a
  Uniform :: GLSLValue a => String -> Var a

varName :: Var a -> String
varName (In s) = s
varName (Out s) = s
varName (Uniform s) = s

data ShaderF a where
  -- Binding
  Bind :: GLSLValue a => Var (Shader a) -> ShaderF (Var (Shader a))

  -- Functions
  Function :: GLSLValue a => String -> [a] -> a -> ShaderF a

  -- Accessors
  Get :: Var (Shader a) -> ShaderF a
  Set :: Var a -> a -> ShaderF a

  -- Literals
  V4 :: GLSLValue a => Linear.V4 a -> ShaderF (Linear.V4 a)

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

type Shader = Freer ShaderF

data Vertex = Vertex { position :: Shader (Linear.V4 Float), pointSize :: Shader Float, clipDistance :: Shader [Float] }

vertex :: Vertex
vertex = Vertex (pure (Linear.V4 0 0 0 0)) (pure 0) (pure [])


uniform :: GLSLValue a => String -> Shader (Var (Shader a))
uniform = liftF . Bind . Uniform

input :: GLSLValue a => String -> Shader (Var (Shader a))
input = liftF . Bind . In

output :: GLSLValue a => String -> Shader (Var (Shader a))
output = liftF . Bind . Out

function :: GLSLValue a => String -> [Shader a] -> Shader a -> Shader a
function name args body = wrap (Function name args body)

get :: Var (Shader a) -> Shader a
get = liftF . Get

set :: Var (Shader a) -> Shader a -> Shader a
set var value = wrap (Set var value)

v4 :: GLSLValue a => a -> a -> a -> a -> Shader (Linear.V4 a)
v4 x y z w = liftF (V4 (Linear.V4 x y z w))

infixl 7 !*

(!*) :: Shader (Linear.M44 a) -> Shader (Linear.V4 a) -> Shader (Linear.V4 a)
matrix !* column = Freer (Free pure (MulMV matrix column))


-- Elaboration

elaborateVertexShader :: Shader Vertex -> Shader ()
elaborateVertexShader shader = do
  Vertex pos _ _ <- shader
  function "main" [] . void $ set gl_Position pos
  where gl_Position = Out "gl_Position"

elaborateShader :: GLSLValue a => Shader a -> Shader ()
elaborateShader shader = do
  out <- output "result"
  function "main" [] . void $ set out shader


-- Compilation

toGLSL :: GLSLValue a => Shader a -> String
toGLSL = ($ "") . (showString "#version 410\n" .) . iterFreer toGLSLAlgebra . fmap showsGLSLValue

toGLSLAlgebra :: forall x. (x -> ShowS) -> ShaderF x -> ShowS
toGLSLAlgebra run shader = case shader of
  Bind var -> case var of
    Uniform s -> showString "uniform" . sp . showsGLSLType (Proxy :: Proxy x) . sp . showString s . showChar ';' . nl . run var
    In s -> showString "in" . sp . showsGLSLType (Proxy :: Proxy x) . sp . showString s . showChar ';' . nl . run var
    Out s -> showString "out" . sp . showsGLSLType (Proxy :: Proxy x) . sp . showString s . showChar ';' . nl . run var

  Function name args body ->
    showsGLSLType (Proxy :: Proxy x) . sp . showString name
    . showParen True (foldr (.) id (intersperse (showString ", ") (if null args then [ showsGLSLType (Proxy :: Proxy ()) ] else run <$> args))) . sp
    . showBrace True (nl . sp . sp . run body)

  Get v -> var v
  Set v value -> var v . sp . showChar '=' . sp . run value . showChar ';' . nl

  V4 v -> showsGLSLValue v

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
        var = showString . varName
        sp = showChar ' '
        nl = showChar '\n'
        vec v = showString "vec" . shows (length v) . showParen True (foldr (.) id (run <$> v))
        recur = (iterFreer toGLSLAlgebra .) . fmap
        showBrace c b = if c then showChar '{' . b . showChar '}' else b


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

class GLSLValue v where
  showsGLSLType :: Proxy v -> ShowS
  showsGLSLVecType :: Proxy v -> ShowS
  showsGLSLValue :: v -> ShowS
  default showsGLSLValue :: Show v => v -> ShowS
  showsGLSLValue = shows

class IsShader t where
  type ShaderResult t :: *

  toShader' :: t -> Int -> Shader (ShaderResult t)

toShader :: IsShader t => t -> Shader (ShaderResult t)
toShader = flip toShader' 0


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
    Bind v -> showsUnaryWith showsPrec "Bind" d v

    Function n a b -> showsTernaryWith showsPrec (liftShowsPrec sp sl) sp "Function" d n a b

    Get v -> showsUnaryWith showsPrec "Get" d v
    Set v value -> showsBinaryWith showsPrec sp "Set" d v value

    V4 v -> showsUnaryWith sp "V4" d v

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
    where showsTernaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> String -> Int -> a -> b -> c -> ShowS
          showsTernaryWith sp1 sp2 sp3 name d x y z = showParen (d > 10) $ showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z


instance GLSLValue () where
  showsGLSLType _ = showString "void"
  showsGLSLVecType _ = showString "void"
  showsGLSLValue = shows

instance GLSLValue Float where
  showsGLSLType _ = showString "float"
  showsGLSLVecType _ = showString "vec4"

instance GLSLValue Bool where
  showsGLSLType _ = showString "bool"
  showsGLSLVecType _ = showString "bvec4"
  showsGLSLValue v = showString $ if v then "true" else "false"

instance GLSLValue a => GLSLValue (Shader a) where
  showsGLSLType _ = showsGLSLType (Proxy :: Proxy a)
  showsGLSLVecType _ = showsGLSLVecType (Proxy :: Proxy a)
  showsGLSLValue _ = id

instance GLSLValue a => GLSLValue (Var a) where
  showsGLSLType _ = showsGLSLType (Proxy :: Proxy a)
  showsGLSLVecType _ = showsGLSLVecType (Proxy :: Proxy a)

instance GLSLValue a => GLSLValue (Linear.V4 a) where
  showsGLSLType _ = showsGLSLVecType (Proxy :: Proxy a)
  showsGLSLVecType _ = showString "mat4"
  showsGLSLValue v = showsGLSLVecType (Proxy :: Proxy a) . showParen True (foldr (.) id (intersperse (showString ", ") (showsGLSLValue <$> toList v)))

instance IsShader (Shader a) where
  type ShaderResult (Shader a) = a
  toShader' = const

instance (GLSLValue a, IsShader b) => IsShader (Var (Shader a) -> b) where
  type ShaderResult (Var (Shader a) -> b) = ShaderResult b
  toShader' f i = do
    var <- input ('v' : show i)
    toShader' (f var) (succ i)
