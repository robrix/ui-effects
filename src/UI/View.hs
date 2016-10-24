module UI.View where

import Control.Monad.Free

-- Datatypes

data ViewF f
  = Text String
  | List [f]
  | Input (String -> f)
  deriving Functor

type View a = Free ViewF a


-- Smart constructors

text :: String -> View ()
text = wrap . Text

list :: [View a] -> View a
list = wrap . List

input :: View String
input = wrap $ Input pure
