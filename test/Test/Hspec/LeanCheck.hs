{-# LANGUAGE FlexibleInstances, GADTs, TypeFamilies #-}
module Test.Hspec.LeanCheck where

import GHC.Stack
import Test.Hspec.Core.Spec
import Test.LeanCheck.Core

data Property where
  Property :: Testable prop => prop -> Property

prop :: (HasCallStack, Testable prop) => String -> prop -> Spec
prop s = it s . Property


instance Example Property where
  type Arg Property = ()
  evaluateExample (Property prop) _ _ _ =
    case counterExample 100 prop of
      Just messages -> pure (Fail Nothing (unlines messages))
      _ -> pure Success
