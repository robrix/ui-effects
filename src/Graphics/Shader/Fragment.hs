{-# LANGUAGE FlexibleInstances, GADTs #-}
module Graphics.Shader.Fragment where

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
  Pure :: a -> Fragment a
  Add, Sub, Mul :: Num a => Fragment a -> Fragment a -> Fragment a
  Abs, Signum:: Num a => Fragment a -> Fragment a
  FromInteger :: Num a => Integer -> Fragment a
  Map :: (a -> b) -> Fragment a -> Fragment b
  App :: Fragment (a -> b) -> Fragment a -> Fragment b

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
  fromInteger = FromInteger

instance Functor Fragment where
  fmap = Map

instance Applicative Fragment where
  pure = Pure
  (<*>) = App
