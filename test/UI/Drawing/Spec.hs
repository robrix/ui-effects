module UI.Drawing.Spec where

import Data.Functor.Algebraic
import Test.Hspec
import UI.Drawing
import UI.Geometry

spec :: Spec
spec = do
  describe "renderingBackgroundRects" $ do
    it "produces a rect for background terms" $
      renderingBackgroundRects (wrapL (Background (rgba 1 1 1 1) (pure 10))) `shouldBe` [ Rect (Point 0 0) (Size 10 10) ]
