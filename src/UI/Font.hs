module UI.Font where

import Control.Exception
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import UI.Geometry
import Opentype.Fileformat

data Typeface = Typeface { typefaceName :: String, typefaceUnderlying :: OpentypeFont }

data Font = Font { fontFace :: Typeface, fontSize :: Int }

readTypeface :: FilePath -> IO (Maybe Typeface)
readTypeface path = (toTypeface <$> readOTFile path) `catch` (\ (SomeException _) -> return Nothing)
  where toTypeface font = do
          name <- opentypeFontName font
          pure $ Typeface name font

opentypeFontName :: OpentypeFont -> Maybe String
opentypeFontName o = T.unpack . T.decodeUtf16BE . nameString <$> find ((== 1) . nameID) (nameRecords (nameTable o))

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
