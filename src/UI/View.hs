module UI.View where

import Control.Monad.Free.Church

-- Datatypes

data ViewF f
  = Text String
  | List [f]
  | Input (String -> f)
  deriving Functor

type View a = F ViewF a


-- Smart constructors

text :: String -> View ()
text = wrap . Text

list :: [View ()] -> View ()
list = wrap . List

input :: View String
input = wrap $ Input pure
