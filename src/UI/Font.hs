module UI.Font where

import UI.Geometry

newtype Typeface = Typeface { typefaceName :: String }

data Font = Font { fontFace :: Typeface, fontSize :: Int }

measureString :: Num a => String -> Size a
measureString s = Size (fromIntegral (length s) * fontW) lineH
  where (fontW, fontH) = (5, 8)
        lineH = fontH + 5

measureStringForWidth :: Real a => a -> String -> Size a
measureStringForWidth maxW s = Size maxW (height line * fromInteger (ceiling (toRational (length s) / (toRational maxW / toRational (width char)))))
  where char = Size 5 8
        line = char + Size 10 5

measureText :: Real a => Maybe a -> String -> Size a
measureText = maybe measureString measureStringForWidth
