module Graphics.Shader.Fragment where

import Control.Applicative.Free.Freer
import Linear.V2
import Linear.V4

data Colour a = RGBA !a !a !a !a

data FragmentF f
  = Coord (V4 Float -> f)
  | SampleID (Int -> f)
  | NumSamples (Int -> f)
  | PointCoord (V2 Float -> f)
  | SamplePosition (V2 Float -> f)
  | SetDepth Float f
  | SetColour (Colour Float) f
  deriving Functor

type Fragment = Freer FragmentF

coord :: Fragment (V4 Float)
coord = liftF $ Coord id

sampleID :: Fragment Int
sampleID = liftF $ SampleID id

numSamples :: Fragment Int
numSamples = liftF $ NumSamples id

pointCoord :: Fragment (V2 Float)
pointCoord = liftF $ PointCoord id

samplePosition :: Fragment (V2 Float)
samplePosition = liftF $ SamplePosition id

setDepth :: Float -> Fragment ()
setDepth d = liftF $ SetDepth d ()

setColour :: Colour Float -> Fragment ()
setColour c = liftF $ SetColour c ()
