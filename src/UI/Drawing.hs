{-# LANGUAGE GADTs #-}
module UI.Drawing where

import Control.Action
import Control.Applicative.Free
import Linear.V2

data Shape a = Rectangle (V2 a) (V2 a)

data Colour a = RGBA !a !a !a !a

data DrawingF a where
  SetStroke :: Colour a -> DrawingF ()
  SetFill :: Colour a -> DrawingF ()
  Stroke :: Shape a -> DrawingF ()
  Fill :: Shape a -> DrawingF ()
  Text :: String -> DrawingF ()

type Drawing a = Ap (Action DrawingF) a

setStroke :: Colour a -> Drawing ()
setStroke c = liftAp . liftAction $ SetStroke c

setFill :: Colour a -> Drawing ()
setFill c = liftAp . liftAction $ SetFill c

stroke :: Shape a -> Drawing ()
stroke s = liftAp . liftAction $ Stroke s

fill :: Shape a -> Drawing ()
fill s = liftAp . liftAction $ Fill s

text :: String -> Drawing ()
text = liftAp . liftAction . Text
