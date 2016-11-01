{-# LANGUAGE GADTs #-}
module UI.Drawing
( Shape(..)
, Colour(..)
, DrawingF(..)
, Drawing
, setStroke
, setFill
, stroke
, fill
, text
, clip
, module Layout
) where

import Control.Action
import Control.Applicative.Free
import Linear.V2
import UI.Layout as Layout

data Shape a = Rectangle (V2 a) (V2 a)

data Colour a = RGBA !a !a !a !a

data DrawingF a where
  SetStroke :: Colour a -> DrawingF ()
  SetFill :: Colour a -> DrawingF ()
  Stroke :: Shape a -> DrawingF ()
  Fill :: Shape a -> DrawingF ()
  Text :: Size (Maybe a) -> String -> DrawingF ()
  Clip :: Size (Maybe a) -> b -> DrawingF ()

type Drawing a = Ap (Action DrawingF) a

setStroke :: Colour a -> Drawing ()
setStroke c = liftAp . liftAction $ SetStroke c

setFill :: Colour a -> Drawing ()
setFill c = liftAp . liftAction $ SetFill c

stroke :: Shape a -> Drawing ()
stroke s = liftAp . liftAction $ Stroke s

fill :: Shape a -> Drawing ()
fill s = liftAp . liftAction $ Fill s

text :: Size (Maybe a) -> String -> Drawing ()
text maxSize = liftAp . liftAction . Text maxSize

clip :: Size (Maybe a) -> Drawing b -> Drawing ()
clip size = liftAp . liftAction . Clip size
