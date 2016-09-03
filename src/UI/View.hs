module UI.View where

data ViewF f
  = Text String
  | List [f]
