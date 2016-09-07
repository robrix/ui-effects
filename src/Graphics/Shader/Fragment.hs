module Graphics.Shader.Fragment where

import Control.Monad.Free.Freer
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

type Fragment = Freer FragmentF

coord :: Fragment (V4 Float)
coord = wrap $ Coord pure

sampleID :: Fragment Int
sampleID = wrap $ SampleID pure

numSamples :: Fragment Int
numSamples = wrap $ NumSamples pure

pointCoord :: Fragment (V2 Float)
pointCoord = wrap $ PointCoord pure

samplePosition :: Fragment (V2 Float)
samplePosition = wrap $ SamplePosition pure

setFragDepth :: Float -> Fragment ()
setFragDepth d = liftF $ SetFragDepth d ()

setFragColour :: Colour Float -> Fragment ()
setFragColour c = liftF $ SetFragColour c ()
