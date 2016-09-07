module Graphics.Shader.Fragment where

import Linear.V2
import Linear.V4

data Colour a = RGBA !a !a !a !a

data FragmentF f
  = Coord (V4 Float -> f)
  | SampleID (Int -> f)
  | NumSamples (Int -> f)
  | PointCoord (V2 Float -> f)
  | SamplePosition (V2 Float -> f)
  | SetFragDepth Float f
  | SetFragColour (Colour Float) f
