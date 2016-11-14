{-# LANGUAGE FlexibleInstances, GADTs, TypeFamilies #-}
module Test.Hspec.LeanCheck where

import GHC.Stack
import Test.Hspec.Core.Spec
import Test.LeanCheck.Core

data Property where
  Property :: Testable prop => prop -> Property

prop :: (HasCallStack, Testable prop) => String -> prop -> Spec
prop s = it s . Property

data ShouldBe where
  ShouldBe :: (Eq a, Show a) => a -> a -> ShouldBe

infix 1 `shouldBe`
shouldBe :: (Eq a, Show a) => a -> a -> ShouldBe
shouldBe = ShouldBe

instance Testable ShouldBe where
  resultiers (ShouldBe actual expected) = fmap prependExpectation <$> resultiers (actual == expected)
    where prependExpectation (strs, False) = (("expected: " ++ show expected ++ "\n but got: " ++ show actual) : strs, False)
          prependExpectation other = other

instance Example Property where
  type Arg Property = ()
  evaluateExample (Property prop) _ _ _ =
    case counterExample 100 prop of
      Just messages -> pure (Fail Nothing (unlines messages))
      _ -> pure Success
