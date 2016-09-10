{-# LANGUAGE ExistentialQuantification, ImplicitParams #-}
module Control.Exception.Located
( module E
, throw
) where

import Control.Exception as E hiding (throw)
import qualified Control.Exception as X
import GHC.SrcLoc
import GHC.Stack

data LocatedException = forall e. Exception e => LocatedException CallStack e

throw :: (Exception e, ?loc :: CallStack) => e -> a
throw = X.throw . LocatedException ?loc

instance Show LocatedException where
  showsPrec p (LocatedException s e) = showsCallStack s . showsPrec p e
    where showsCallStack = foldr (.) id . fmap showsLoc . tail . getCallStack
          showsLoc (function, location) = foldr (.) id
            [ showString (srcLocFile location)
            , showChar ':'
            , shows (srcLocStartLine location)
            , showChar ':'
            , shows (srcLocStartCol location)
            , showChar '-'
            , shows (srcLocEndLine location)
            , showChar ':'
            , shows (srcLocEndCol location)
            , showChar ':'
            , showParen True (showString function)
            , showChar '\n' ]

instance Exception LocatedException
