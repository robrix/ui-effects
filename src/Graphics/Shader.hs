{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, KindSignatures, StandaloneDeriving #-}
module Graphics.Shader where

import qualified Linear.V2 as Linear
import qualified Linear.V3 as Linear
import qualified Linear.V4 as Linear

type Colour = Linear.V4

data ShaderType = Fragment | Vertex
data VarType = In | Out | Uniform

data Var (t :: VarType) (k :: ShaderType) a where
  Var :: String -> Var t k a

  Position :: Var 'Out 'Vertex (Linear.V4 Float)
  PointSize :: Var 'Out 'Vertex Float

  Coord :: Var 'In 'Fragment (Linear.V4 Float)
  PointCoord :: Var 'In 'Fragment (Linear.V2 Float)
  FrontFacing :: Var 'In 'Fragment Bool
  Depth :: Var 'Out 'Fragment Float

data Shader (k :: ShaderType) t where
  Lambda :: String -> (Var 'In k a -> Shader k b) -> Shader k b
  Get :: Var 'In k a -> Shader k a
  Set :: Var 'Out k a -> Shader k a -> Shader k a

  -- Literals
  Scalar :: Show a => a -> Shader k a
  V2 :: Show a => a -> a -> Shader k (Linear.V2 a)
  V3 :: Show a => a -> a -> a -> Shader k (Linear.V3 a)
  V4 :: Show a => a -> a -> a -> a -> Shader k (Linear.V4 a)

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
lambda = Lambda

get :: Var 'In k a -> Shader k a
get = Get

set :: Var 'Out k a -> Shader k a -> Shader k a
set = Set

out :: String -> Var 'Out k a
out = Var

uniform :: String -> Var 'Uniform k a
uniform = Var


instance Num a => Num (Shader k a) where
  (+) = Add
  (-) = Sub
  (*) = Mul
  abs = Abs
  signum = Signum
  fromInteger = FromRational . fromInteger

instance Fractional a => Fractional (Shader k a) where
  (/) = Div
  fromRational = FromRational

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

instance Eq a => Eq (Shader k a) where
  Lambda s1 f1 == Lambda s2 f2 = s1 == s2 && f1 (Var s1) == f2 (Var s2)
  Get v1 == Get v2 = v1 == v2
  Set v1 p1 == Set v2 p2 = v1 == v2 && p1 == p2

  Scalar x1 == Scalar x2 = x1 == x2
  V2 x1 y1 == V2 x2 y2 = Linear.V2 x1 y1 == Linear.V2 x2 y2
  V3 x1 y1 z1 == V3 x2 y2 z2 = Linear.V3 x1 y1 z1 == Linear.V3 x2 y2 z2
  V4 x1 y1 z1 w1 == V4 x2 y2 z2 w2 = Linear.V4 x1 y1 z1 w1 == Linear.V4 x2 y2 z2 w2

  Add x1 y1 == Add x2 y2 = x1 == x2 && y1 == y2
  Sub x1 y1 == Sub x2 y2 = x1 == x2 && y1 == y2
  Mul x1 y1 == Mul x2 y2 = x1 == x2 && y1 == y2
  Div x1 y1 == Div x2 y2 = x1 == x2 && y1 == y2

  Abs x1 == Abs x2 = x1 == x2
  Signum x1 == Signum x2 = x1 == x2
  FromRational x1 == FromRational x2 = x1 == x2

  Sin x1 == Sin x2 = x1 == x2
  Cos x1 == Cos x2 = x1 == x2
  Tan x1 == Tan x2 = x1 == x2
  ASin x1 == ASin x2 = x1 == x2
  ACos x1 == ACos x2 = x1 == x2
  ATan x1 == ATan x2 = x1 == x2
  SinH x1 == SinH x2 = x1 == x2
  CosH x1 == CosH x2 = x1 == x2
  TanH x1 == TanH x2 = x1 == x2
  ASinH x1 == ASinH x2 = x1 == x2
  ACosH x1 == ACosH x2 = x1 == x2
  ATanH x1 == ATanH x2 = x1 == x2

  Exp x1 == Exp x2 = x1 == x2
  Log x1 == Log x2 = x1 == x2

  _ == _ = False
