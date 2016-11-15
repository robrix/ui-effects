module Data.Functor.Pretty where

import Text.PrettyPrint.Free

class Pretty1 f where
  liftPretty :: (a -> Doc e) -> f a -> Doc e

pretty1 :: (Pretty1 f, Pretty a) => f a -> Doc e
pretty1 = liftPretty pretty
