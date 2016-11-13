module UI.Layout.Spec where

import Test.Hspec
import Test.Hspec.LeanCheck
import UI.Geometry
import UI.Layout

spec :: Spec
spec = do
  describe "fitLayout" $ do
    prop "excludes sizes wider than the horizontal maximum" $
      \ w -> fitLayout (Size (Just w) Nothing) (pure (Size (w + 1 :: Int) 0)) == Nothing
