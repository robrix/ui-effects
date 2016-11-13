module UI.Layout.Spec where

import Data.Maybe (isJust)
import Test.Hspec
import Test.Hspec.LeanCheck
import UI.Geometry
import UI.Layout

spec :: Spec
spec = do
  describe "fitLayout" $ do
    prop "includes only sizes up to the horizontal maximum" $
      \ maxW w -> isJust (fitLayout (Size (Just maxW) Nothing) (pure (Size (w :: Int) 0))) == (maxW >= w)

    prop "includes only sizes up to the vertical maximum" $
      \ maxH h -> isJust (fitLayout (Size Nothing (Just maxH)) (pure (Size 0 (h :: Int)))) == (maxH >= h)
