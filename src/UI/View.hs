module UI.View where

import Control.Monad.Free.Church

data ViewF f
  = Text String
  | List [f]
  deriving Functor

type View a = F ViewF a

text :: String -> View a
text = wrap . Text

list :: [View a] -> View a
list = wrap . List
