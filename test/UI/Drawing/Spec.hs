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

    it "respects containing insets" $
      renderingBackgroundRects (wrapR (Inset 10 (wrapL (Background (rgba 1 1 1 1) (pure 10))))) `shouldBe` [ Rect (Point 10 10) (Size 10 10) ]

    it "respects contained insets" $
      renderingBackgroundRects (wrapL (Background (rgba 1 1 1 1) (wrapR (Inset 10 (pure 10))))) `shouldBe` [ Rect (Point 0 0) (Size 30 30) ]

    it "respects indirect nesting" $
      renderingBackgroundRects (wrapL (Background (rgba 1 1 1 1) (wrapR (Inset 10 (wrapL (Background (rgba 0 0 0 1) (pure 10))))))) `shouldBe` [ Rect (Point 0 0) (Size 30 30), Rect (Point 10 10) (Size 10 10) ]
