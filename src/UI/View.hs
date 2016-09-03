module UI.View where

data View f
  = Text String
  | List [f]
