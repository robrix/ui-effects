{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, KindSignatures, StandaloneDeriving #-}
module Graphics.Shader where

import qualified Linear.V2 as Linear
import qualified Linear.V3 as Linear
import qualified Linear.V4 as Linear

type Colour = Linear.V4

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
  FromRational :: Num a => Rational -> Shader k a

  -- Trigonometric
  Sin, Cos, Tan :: Num a => Shader k a -> Shader k a
  ASin, ACos, ATan :: Num a => Shader k a -> Shader k a
  SinH, CosH, TanH :: Num a => Shader k a -> Shader k a
  ASinH, ACosH, ATanH :: Num a => Shader k a -> Shader k a

  Exp, Log :: Num a => Shader k a -> Shader k a

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

uniform :: String -> Var 'In k a
uniform = Uniform


v2 :: Show a => a -> a -> Shader k (Linear.V2 a)
v2 x y = V2 $ Linear.V2 x y

v3 :: Show a => a -> a -> a -> Shader k (Linear.V3 a)
v3 x y z = V3 $ Linear.V3 x y z

v4 :: Show a => a -> a -> a -> a -> Shader k (Linear.V4 a)
v4 x y z w = V4 $ Linear.V4 x y z w


instance Num a => Num (Shader k a) where
  (+) = Add
  (-) = Sub
  (*) = Mul
  abs = Abs
  signum = Signum
  fromInteger = FromRational . fromInteger

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
