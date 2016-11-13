module UI.Layout.Spec where

import Test.Hspec
import UI.Geometry
import UI.Layout

spec :: Spec
spec = do
  describe "fitLayout" $ do
    it "excludes sizes larger than the horizontal maximum" $
      fitLayout (Size (Just 10) Nothing) (pure (Size 11 11)) `shouldBe` Nothing
