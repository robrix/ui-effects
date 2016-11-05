{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, KindSignatures, RankNTypes, StandaloneDeriving #-}
module GL.Shader
( Var
, Shader
, ShaderType(..)
, position
, pointSize
, coord
, pointCoord
, frontFacing
, depth
, lambda
, set
, get
, out
, uniform
, mat4Uniform
, v2
, v3
, v4
, GLShader(..)
, toGLSL
, withCompiledShaders
) where

import Control.Exception
import Control.Monad.Free.Freer
import Data.List (intersperse)
import Data.Monoid ((<>))
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GL.Exception
import Graphics.GL.Core41
import Graphics.GL.Types
import qualified Linear.Matrix as Linear
import qualified Linear.V2 as Linear
import qualified Linear.V3 as Linear
import qualified Linear.V4 as Linear
import Prelude hiding (IO)

data ShaderType = Fragment | Vertex
data VarType = In | Out

data Var (t :: VarType) (k :: ShaderType) a where
  Var :: String -> Var t k a
  Uniform :: String -> Var t k a

  Position :: Var 'Out 'Vertex (Linear.V4 Float)
  PointSize :: Var 'Out 'Vertex Float

  Coord :: Var 'In 'Fragment (Linear.V4 Float)
  PointCoord :: Var 'In 'Fragment (Linear.V2 Float)
  FrontFacing :: Var 'In 'Fragment Bool
  Depth :: Var 'Out 'Fragment Float

data Shader (k :: ShaderType) t where
  Lambda :: String -> Shader k a -> Shader k a
  Get :: Var 'In k a -> Shader k a
  Set :: Var 'Out k a -> Shader k a -> Shader k a

  -- Literals
  Scalar :: Show a => a -> Shader k a
  V2 :: Show a => Linear.V2 a -> Shader k (Linear.V2 a)
  V3 :: Show a => Linear.V3 a -> Shader k (Linear.V3 a)
  V4 :: Show a => Linear.V4 a -> Shader k (Linear.V4 a)

  -- Arithmetic
  Add, Sub, Mul, Div :: Num a => Shader k a -> Shader k a -> Shader k a
  Abs, Signum :: Num a => Shader k a -> Shader k a

  -- Trigonometric
  Sin, Cos, Tan :: Num a => Shader k a -> Shader k a
  ASin, ACos, ATan :: Num a => Shader k a -> Shader k a
  SinH, CosH, TanH :: Num a => Shader k a -> Shader k a
  ASinH, ACosH, ATanH :: Num a => Shader k a -> Shader k a

  Exp, Log :: Num a => Shader k a -> Shader k a

data ShaderF a where
  Add', Sub', Mul', Div' :: a -> a -> ShaderF a
  Abs', Signum' :: a -> ShaderF a

  Sin', Cos', Tan' :: a -> ShaderF a
  ASin', ACos', ATan' :: a -> ShaderF a
  SinH', CosH', TanH' :: a -> ShaderF a
  ASinH', ACosH', ATanH' :: a -> ShaderF a

  Exp', Log' :: a -> ShaderF a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

type Shader' = Freer ShaderF


position :: Var 'Out 'Vertex (Linear.V4 Float)
position = Position

pointSize :: Var 'Out 'Vertex Float
pointSize = PointSize

coord :: Var 'In 'Fragment (Linear.V4 Float)
coord = Coord

pointCoord :: Var 'In 'Fragment (Linear.V2 Float)
pointCoord = PointCoord

frontFacing :: Var 'In 'Fragment Bool
frontFacing = FrontFacing

depth :: Var 'Out 'Fragment Float
depth = Depth

lambda :: String -> (Var 'In k a -> Shader k b) -> Shader k b
lambda s f = Lambda s (f (Var s))

get :: Var 'In k a -> Shader k a
get = Get

set :: Var 'Out k a -> Shader k a -> Shader k a
set = Set

out :: String -> Var 'Out k a
out = Var

uniform :: String -> Shader k a
uniform = Get . Uniform

mat4Uniform :: String -> Shader k (Linear.M44 a)
mat4Uniform = Get . Uniform


v2 :: Show a => a -> a -> Shader k (Linear.V2 a)
v2 x y = V2 $ Linear.V2 x y

v3 :: Show a => a -> a -> a -> Shader k (Linear.V3 a)
v3 x y z = V3 $ Linear.V3 x y z

v4 :: Show a => a -> a -> a -> a -> Shader k (Linear.V4 a)
v4 x y z w = V4 $ Linear.V4 x y z w


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
        go (Sin a) = showString "sin" . showParen True (go a)
        go (Abs a) = showString "abs" . showParen True (go a)
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
        inputs (Div a b) = inputs a <> inputs b
        inputs _ = []

        outputs :: Shader k a -> [ShowS]
        outputs (Set (Var s) c) = showString ("out vec4 " <> s <> ";") : outputs c
        outputs (Lambda _ a) = outputs a
        outputs (Add a b) = outputs a <> outputs b
        outputs (Mul a b) = outputs a <> outputs b
        outputs (Div a b) = outputs a <> outputs b
        outputs _ = []

        uniforms :: Shader k a -> [ShowS]
        uniforms (Set _ v) = uniforms v
        uniforms (Lambda _ a) = uniforms a
        uniforms (Get (Uniform s)) = [ showString $ "uniform vec4 " <> s <> ";" ]
        uniforms (Add a b) = uniforms a <> uniforms b
        uniforms (Mul a b) = uniforms a <> uniforms b
        uniforms (Div a b) = uniforms a <> uniforms b
        uniforms (Sin a) = uniforms a
        uniforms (Abs a) = uniforms a
        uniforms _ = []

        pragma k v = showString $ "#" <> k <> " " <> v <> "\n"
        main body = showString "void main(void) {\n" . body . showString "}"

instance (Show a, Num a) => Num (Shader k a) where
  (+) = Add
  (-) = Sub
  (*) = Mul
  abs = Abs
  signum = Signum
  fromInteger = Scalar . fromInteger

instance (Show a, Fractional a) => Fractional (Shader k a) where
  (/) = Div
  fromRational = Scalar . fromRational

instance (Show a, Floating a) => Floating (Shader k a) where
  sin = Sin
  cos = Cos
  tan = Tan
  asin = ASin
  acos = ACos
  atan = ATan
  sinh = SinH
  cosh = CosH
  tanh = TanH
  asinh = ASinH
  acosh = ACosH
  atanh = ATanH

  pi = Scalar pi
  exp = Exp
  log = Log

deriving instance Eq (Var t k a)
deriving instance Ord (Var t k a)
deriving instance Eq a => Eq (Shader k a)
deriving instance Ord a => Ord (Shader k a)
deriving instance Foldable (Var io k)
deriving instance Foldable (Shader k)


instance Num a => Num (Shader' a) where
  (+) = (wrap .) . Add'
  (-) = (wrap .) . Sub'
  (*) = (wrap .) . Mul'

  abs = wrap . Abs'
  signum = wrap . Signum'
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (Shader' a) where
  (/) = (wrap .) . Div'
  fromRational = pure . fromRational
