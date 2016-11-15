module Data.Functor.Pretty where

import Text.PrettyPrint.Free

class Pretty1 f where
  liftPretty :: (a -> Doc e) -> f a -> Doc e
