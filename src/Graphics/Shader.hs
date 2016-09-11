{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, KindSignatures #-}
module Graphics.Shader where

import Linear.V2
import Linear.V4

type Colour = V4

data ShaderType = Fragment | Vertex

data Shader (k :: ShaderType) t where
  -- Globals
  Coord :: Shader k (V4 Float)
  SampleID :: Shader k Int
  NumSamples :: Shader k Int
  PointCoord :: Shader k (V2 Float)
  Position :: Shader 'Vertex (V4 Float)
  SamplePosition :: Shader 'Fragment (V2 Float)
  SetDepth :: Shader 'Fragment Float -> Shader 'Fragment ()
  SetColour :: Shader 'Fragment (Colour Float) -> Shader 'Fragment ()
  SetPosition :: Shader 'Vertex (V4 Float) -> Shader 'Vertex ()

  -- Literals
  V2 :: Show a => a -> a -> Shader k (V2 a)
  V4 :: Show a => a -> a -> a -> a -> Shader k (V4 a)

  -- Arithmetic
  Add, Sub, Mul, Div :: Num a => Shader k a -> Shader k a -> Shader k a
  Abs, Signum :: Num a => Shader k a -> Shader k a
  FromRational :: Num a => Rational -> Shader k a

coord :: Shader k (V4 Float)
coord = Coord

sampleID :: Shader k Int
sampleID = SampleID

numSamples :: Shader k Int
numSamples = NumSamples

pointCoord :: Shader k (V2 Float)
pointCoord = PointCoord

position :: Shader 'Vertex (V4 Float)
position = Position

samplePosition :: Shader 'Fragment (V2 Float)
samplePosition = SamplePosition

setDepth :: Shader 'Fragment Float -> Shader 'Fragment ()
setDepth = SetDepth

setColour :: Shader 'Fragment (Colour Float) -> Shader 'Fragment ()
setColour = SetColour

setPosition :: Shader 'Vertex (V4 Float) -> Shader 'Vertex ()
setPosition = SetPosition


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
