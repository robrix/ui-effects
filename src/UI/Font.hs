module UI.Font where

newtype Typeface = Typeface { typefaceName :: String }

data Font = Font { fontFace :: Typeface, fontSize :: Int }
