module Data.Functor.Pretty
( module P
, Pretty(..)
, pretty
, Pretty1(..)
, prettyPrec1
, Pretty2(..)
, prettyPrec2
, prettyParen
, (</>)
) where

import Data.Char (showLitChar)
import Text.PrettyPrint.Free as P hiding (Pretty(..), pretty, (</>))
import qualified Text.PrettyPrint.Free as PP

class Pretty a where
  prettyPrec :: Int -> a -> Doc e

pretty :: Pretty a => a -> Doc e
pretty = prettyPrec 0


class Pretty1 f where
  liftPrettyPrec :: (Int -> a -> Doc e) -> Int -> f a -> Doc e

prettyPrec1 :: (Pretty1 f, Pretty a) => Int -> f a -> Doc e
prettyPrec1 = liftPrettyPrec prettyPrec


class Pretty2 f where
  liftPrettyPrec2 :: (Int -> a -> Doc e) -> (Int -> b -> Doc e) -> Int -> f a b -> Doc e

prettyPrec2 :: (Pretty2 f, Pretty a, Pretty b) => Int -> f a b -> Doc e
prettyPrec2 = liftPrettyPrec2 prettyPrec prettyPrec


prettyParen :: Bool -> Doc e -> Doc e
prettyParen True = parens
prettyParen False = id


(</>) :: Doc e -> Doc e -> Doc e
(</>) = (hang 2 .) . (PP.</>)


-- Instances

instance Pretty1 Maybe where
  liftPrettyPrec p1 d = prettyParen (d > 10) . maybe (text "Nothing") ((text "Just" </>) . p1 11)

instance Pretty a => Pretty (Maybe a) where
  prettyPrec = prettyPrec1

instance Pretty1 [] where
  liftPrettyPrec p _ = list . fmap (p 0)

instance Pretty a => Pretty [a] where
  prettyPrec = prettyPrec1

instance Pretty Int where
  prettyPrec = const PP.pretty

instance Pretty Bool where
  prettyPrec = const PP.pretty

instance Pretty Char where
  prettyPrec = const (squotes . text . ($ "") . showLitChar)
