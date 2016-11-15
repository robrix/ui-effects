module Data.Functor.Pretty
( module PP
, Pretty1(..)
, pretty1
, Pretty2(..)
, pretty2
) where

import Text.PrettyPrint.Free as PP

class Pretty1 f where
  liftPretty :: (a -> Doc e) -> f a -> Doc e

pretty1 :: (Pretty1 f, Pretty a) => f a -> Doc e
pretty1 = liftPretty pretty


class Pretty2 f where
  liftPretty2 :: (a -> Doc e) -> (b -> Doc e) -> f a b -> Doc e

pretty2 :: (Pretty2 f, Pretty a, Pretty b) => f a b -> Doc e
pretty2 = liftPretty2 pretty pretty
