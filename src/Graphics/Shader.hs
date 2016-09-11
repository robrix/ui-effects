{-# LANGUAGE FlexibleInstances, GADTs #-}
module Graphics.Shader where

import Linear.V2
import Linear.V4

type Colour = V4

data Shader t where
  -- Globals
  Coord :: Shader (V4 Float)
  SampleID :: Shader Int
  NumSamples :: Shader Int
  PointCoord :: Shader (V2 Float)
  SamplePosition :: Shader (V2 Float)
  SetDepth :: Shader Float -> Shader ()
  SetColour :: Shader (Colour Float) -> Shader ()

  -- Literals
  V2 :: Show a => a -> a -> Shader (V2 a)
  V4 :: Show a => a -> a -> a -> a -> Shader (V4 a)

  -- Arithmetic
  Add, Sub, Mul, Div :: Num a => Shader a -> Shader a -> Shader a
  Abs, Signum :: Num a => Shader a -> Shader a
  FromRational :: Num a => Rational -> Shader a

coord :: Shader (V4 Float)
coord = Coord

sampleID :: Shader Int
sampleID = SampleID

numSamples :: Shader Int
numSamples = NumSamples

pointCoord :: Shader (V2 Float)
pointCoord = PointCoord

samplePosition :: Shader (V2 Float)
samplePosition = SamplePosition

setDepth :: Shader Float -> Shader ()
setDepth = SetDepth

setColour :: Shader (Colour Float) -> Shader ()
setColour = SetColour


instance Num a => Num (Shader a) where
  (+) = Add
  (-) = Sub
  (*) = Mul
  abs = Abs
  signum = Signum
  fromInteger = FromRational . fromInteger

instance Fractional a => Fractional (Shader a) where
  (/) = Div
  fromRational = FromRational
