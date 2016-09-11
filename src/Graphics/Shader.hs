{-# LANGUAGE FlexibleInstances, GADTs #-}
module Graphics.Shader where

import Linear.V2
import Linear.V4

type Colour = V4

data Fragment t where
  Coord :: Fragment (V4 Float)
  SampleID :: Fragment Int
  NumSamples :: Fragment Int
  PointCoord :: Fragment (V2 Float)
  SamplePosition :: Fragment (V2 Float)
  SetDepth :: Fragment Float -> Fragment ()
  SetColour :: Fragment (Colour Float) -> Fragment ()
  V2 :: Show a => a -> a -> Fragment (V2 a)
  V4 :: Show a => a -> a -> a -> a -> Fragment (V4 a)
  Add, Sub, Mul, Div :: Num a => Fragment a -> Fragment a -> Fragment a
  Abs, Signum :: Num a => Fragment a -> Fragment a
  FromRational :: Num a => Rational -> Fragment a

coord :: Fragment (V4 Float)
coord = Coord

sampleID :: Fragment Int
sampleID = SampleID

numSamples :: Fragment Int
numSamples = NumSamples

pointCoord :: Fragment (V2 Float)
pointCoord = PointCoord

samplePosition :: Fragment (V2 Float)
samplePosition = SamplePosition

setDepth :: Fragment Float -> Fragment ()
setDepth = SetDepth

setColour :: Fragment (Colour Float) -> Fragment ()
setColour = SetColour


instance Num a => Num (Fragment a) where
  (+) = Add
  (-) = Sub
  (*) = Mul
  abs = Abs
  signum = Signum
  fromInteger = FromRational . fromInteger

instance Fractional a => Fractional (Fragment a) where
  (/) = Div
  fromRational = FromRational
