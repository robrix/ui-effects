{-# LANGUAGE DeriveFunctor, ScopedTypeVariables #-}
module UI.Font where

import Control.Exception
import Control.Monad
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Int
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import Data.Vector ((!?))
import Opentype.Fileformat hiding (nameID, unitsPerEm, ascent, descent)
import qualified Opentype.Fileformat as O
import UI.Geometry

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
opentypeFontName o = T.unpack . T.decodeUtf16BE . nameString <$> find ((== Just FullName) . nameID) (nameRecords (nameTable o))

nameID :: NameRecord -> Maybe NameID
nameID = safeToEnum . fromIntegral . O.nameID

glyphsForChars :: Typeface -> [Char] -> [Maybe (Glyph Int)]
glyphsForChars (Typeface _ o) chars = map (>>= (glyphs !?) . fromIntegral) glyphIDs
  where glyphIDs = fromMaybe (Nothing <$ chars) $ do
          cmap <- find viablePlatform (getCmaps (cmapTable o))
          Just $ lookupAll (glyphMap cmap) (fmap (fromIntegral . ord :: Char -> Word32) chars)
        lookupAll = fmap . flip Map.lookup
        QuadTables _ (GlyfTable glyphs) = outlineTables o
        viablePlatform p = cmapPlatform p == UnicodePlatform || cmapPlatform p == MicrosoftPlatform && cmapEncoding p == 1

unitsPerEm :: Typeface -> Word16
unitsPerEm = O.unitsPerEm . headTable . typefaceUnderlying

ascent :: Typeface -> Int16
ascent = O.ascent . hheaTable . typefaceUnderlying

descent :: Typeface -> Int16
descent = O.descent . hheaTable . typefaceUnderlying

safeToEnum :: forall n. (Bounded n, Enum n, Ord n) => Int -> Maybe n
safeToEnum n = do
  guard (n < fromEnum (maxBound :: n))
  guard (n > fromEnum (minBound :: n))
  pure (toEnum n)

data Path n = M n n (Path n) | L n n (Path n) | Q n n n n (Path n) | Z
  deriving (Eq, Functor, Show)

contourToPath :: [CurvePoint] -> Path FWord
contourToPath [] = Z
contourToPath (p@(CurvePoint x y _) : rest) = makePath Z
  where (makePath, _) = (foldl (\ (makePath, prev) point -> (makePath . pathFor prev point, point)) (M x y, p) rest)
        pathFor (CurvePoint _ _ True)  (CurvePoint _ _ False)   = id
        pathFor (CurvePoint _ _ True)  (CurvePoint x y True)    = L x y
        pathFor (CurvePoint x y False) (CurvePoint x' y' False) = Q x y (x + ((x' - x) `div` 2)) (y + ((y' - y) `div` 2))
        pathFor (CurvePoint x y False) (CurvePoint x' y' True)  = Q x y x' y'

glyphPaths :: Glyph Int -> [Path FWord]
glyphPaths (Glyph { glyphOutlines = GlyphContours contours _ }) = fmap contourToPath contours
glyphPaths _ = []

compilePath :: Path FWord -> [Word8]
compilePath = go . fmap (word16Bytes . fromIntegral)
  where go path = case path of
          M x y       rest -> moveTo  : x ++ y             ++ go rest
          L x y       rest -> lineTo  : x ++ y             ++ go rest
          Q x y x' y' rest -> curveTo : x ++ y ++ x' ++ y' ++ go rest
          _                -> close   : []
        [moveTo, lineTo, curveTo, close] = [0..3]

word16Bytes :: Word16 -> [Word8]
word16Bytes x = [ fromIntegral $ x .&. 0xFF, fromIntegral $ (x .&. 0xFF00) `shiftR` 8 ]

word32Bytes :: Word32 -> [Word8]
word32Bytes x = [ fromIntegral $ x .&. 0xFF, fromIntegral $ (x .&. 0xFF00) `shiftR` 8, fromIntegral $ (x .&. 0xFF0000) `shiftR` 16, fromIntegral $ (x .&. 0xFF000000) `shiftR` 24 ]


compileGlyphPaths :: Glyph Int -> [Word8]
compileGlyphPaths = (>>= compilePath) . glyphPaths


compileGlyphsForChars :: Typeface -> [Char] -> [Word8]
compileGlyphsForChars face chars = header ++ glyphHeaders ++ (charsGlyphsAndPaths >>= \ (_, _, path) -> path)
  where charsGlyphsAndPaths = zip chars (glyphsForChars face chars) >>= \ (char, glyph) -> (,,) char <$> toList glyph <*> fmap compileGlyphPaths (toList glyph)
        header = word16Bytes (unitsPerEm face) ++ word16Bytes (fromIntegral (ascent face)) ++ word16Bytes (fromIntegral (descent face)) ++ word16Bytes (fromIntegral (length charsGlyphsAndPaths))
        glyphHeaders = snd (foldl compileGlyphHeader (0, id) charsGlyphsAndPaths) []
        compileGlyphHeader (offset, makeList) (char, glyph, path) = (offset + fromIntegral (length path), makeList . (++ (word16Bytes (fromIntegral (ord char)) ++ word16Bytes (advanceWidth glyph) ++ word32Bytes offset ++ word16Bytes (fromIntegral (length path)))))

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
