{-# LANGUAGE GADTs #-}
module UI.Drawing
( Shape(..)
, Colour(..)
, DrawingF(..)
, Drawing
, Rendering
, setStroke
, setFill
, stroke
, fill
, text
, clip
, module Layout
) where

import Control.Action
import Control.Monad.Free.Church
import Data.Functor.Sum
import qualified Linear.V2 as Linear
import UI.Layout as Layout
import UI.Geometry

data Shape a = Rectangle (Linear.V2 a) (Linear.V2 a)

data Colour a = RGBA !a !a !a !a

data DrawingF a where
  SetStroke :: Colour a -> DrawingF ()
  SetFill :: Colour a -> DrawingF ()
  Stroke :: Shape a -> DrawingF ()
  Fill :: Shape a -> DrawingF ()
  Text :: Size (Maybe a) -> String -> DrawingF ()
  Clip :: Size a -> b -> DrawingF ()

type Drawing a = F (Action DrawingF) a
type Rendering a = F (Sum (Action DrawingF) (LayoutF a))

setStroke :: Colour a -> Drawing ()
setStroke c = liftF . liftAction $ SetStroke c

setFill :: Colour a -> Drawing ()
setFill c = liftF . liftAction $ SetFill c

stroke :: Shape a -> Drawing ()
stroke s = liftF . liftAction $ Stroke s

fill :: Shape a -> Drawing ()
fill s = liftF . liftAction $ Fill s

text :: Size (Maybe a) -> String -> Drawing ()
text maxSize = liftF . liftAction . Text maxSize

clip :: Size a -> Drawing b -> Drawing ()
clip size = liftF . liftAction . Clip size
-- renderingVertices = iter $ \ r -> case r of
--   InL (Action drawing run) -> run drawing
--   InR _ -> _
