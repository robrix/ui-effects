module UI.Layout.Spec where

import Data.Maybe (isJust, isNothing)
import Test.Hspec
import Test.Hspec.LeanCheck
import UI.Geometry
import UI.Layout

spec :: Spec
spec = do
  describe "fitLayout" $ do
    prop "excludes sizes wider than the horizontal maximum" $
      \ w -> isNothing $ fitLayout (Size (Just w) Nothing) (pure (Size (w + 1 :: Int) 0))

    prop "includes sizes as wide as the horizontal maximum" $
      \ w -> isJust $ fitLayout (Size (Just w) Nothing) (pure (Size (w :: Int) 0))
