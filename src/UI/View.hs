module UI.View where

import Data.Functor.Foldable

data ViewF f
  = Text String
  | List [f]

type View = Fix ViewF
