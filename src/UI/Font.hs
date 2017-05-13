{-# LANGUAGE ScopedTypeVariables #-}
module UI.Font where

import Control.Exception
import Control.Monad
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import UI.Geometry
import Opentype.Fileformat hiding (nameID)
import qualified Opentype.Fileformat as O

data Typeface = Typeface { typefaceName :: String, typefaceUnderlying :: OpentypeFont }

data Font = Font { fontFace :: Typeface, fontSize :: Int }

readTypeface :: FilePath -> IO (Maybe Typeface)
readTypeface path = (toTypeface <$> readOTFile path) `catch` (\ (SomeException _) -> return Nothing)
  where toTypeface font = do
          name <- opentypeFontName font
          pure $ Typeface name font

data NameID = Copyright | FamilyName | SubfamilyName | UniqueID | FullName | Version | PostScriptName | Trademark | ManufacturerName | Designer | Description | VendorURL | DesignerURL | LicenseDescription | LicenseURL | Reserved | TypographicFamilyName | TypographicSubfamilyName | CompatibleFullName | SampleText | PostScriptCIDFindFontName | WWSFamilyName | WWSSubfamilyName | LightBackgroundPalette | DarkBackgroundPalette | VariationsPostScriptNamePrefix
  deriving (Bounded, Enum, Eq, Ord, Show)

opentypeFontName :: OpentypeFont -> Maybe String
opentypeFontName o = T.unpack . T.decodeUtf16BE . nameString <$> find ((== FullName) . nameID) (nameRecords (nameTable o))

nameID :: NameRecord -> NameID
nameID = toEnum . fromIntegral . O.nameID

safeToEnum :: forall n. (Bounded n, Enum n, Ord n) => Int -> Maybe n
safeToEnum n = do
  guard (n < fromEnum (maxBound :: n))
  guard (n > fromEnum (minBound :: n))
  pure (toEnum n)

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


instance Show Typeface where
  showsPrec d (Typeface name _) = showParen (d > 10) $ showString "Typeface { typefaceName = " . shows name . showString ", typefaceUnderlying = _ }"
