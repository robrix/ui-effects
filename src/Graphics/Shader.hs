{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, KindSignatures #-}
module Graphics.Shader where

import Linear.V2
import Linear.V4

type Colour = V4

data ShaderType = Fragment | Vertex
data VarType = In | Out

data Var (t :: VarType) (k :: ShaderType) a where
  Var :: String -> Var t k a

  Position :: Var 'Out 'Vertex (V4 Float)
  PointSize :: Var 'Out 'Vertex Float

  Coord :: Var 'In 'Fragment (V4 Float)
  PointCoord :: Var 'In 'Fragment (V2 Float)
  FrontFacing :: Var 'In 'Fragment Bool
  Depth :: Var 'Out 'Fragment Float

data Shader (k :: ShaderType) t where
  -- Globals
  SampleID :: Shader k Int
  NumSamples :: Shader k Int
  SamplePosition :: Shader 'Fragment (V2 Float)
  SetColour :: Shader 'Fragment (Colour Float) -> Shader 'Fragment ()

  Lambda :: String -> (Var 'In k a -> Shader k b) -> Shader k b
  Get :: Var 'In k a -> Shader k a
  Set :: Var 'Out k a -> Shader k a -> Shader k ()

  -- Literals
  V2 :: Show a => a -> a -> Shader k (V2 a)
  V4 :: Show a => a -> a -> a -> a -> Shader k (V4 a)

  -- Arithmetic
  Add, Sub, Mul, Div :: Num a => Shader k a -> Shader k a -> Shader k a
  Abs, Signum :: Num a => Shader k a -> Shader k a
  FromRational :: Num a => Rational -> Shader k a

sampleID :: Shader k Int
sampleID = SampleID

numSamples :: Shader k Int
numSamples = NumSamples

position :: Var 'Out 'Vertex (V4 Float)
position = Position

pointSize :: Var 'Out 'Vertex Float
pointSize = PointSize

coord :: Var 'In 'Fragment (V4 Float)
coord = Coord

pointCoord :: Var 'In 'Fragment (V2 Float)
pointCoord = PointCoord

frontFacing :: Var 'In 'Fragment Bool
frontFacing = FrontFacing

depth :: Var 'Out 'Fragment Float
depth = Depth

samplePosition :: Shader 'Fragment (V2 Float)
samplePosition = SamplePosition

setColour :: Shader 'Fragment (Colour Float) -> Shader 'Fragment ()
setColour = SetColour

lambda :: String -> (Var 'In k a -> Shader k b) -> Shader k b
lambda = Lambda

get :: Var 'In k a -> Shader k a
get = Get

set :: Var 'Out k a -> Shader k a -> Shader k ()
set = Set

out :: String -> Var 'Out k a
out = Var


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
