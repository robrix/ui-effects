module GL.Shader.Fragment.Spec where

import GL.Shader.Fragment
import Graphics.Shader.Fragment
import Linear.V4
import Test.Hspec

spec :: Spec
spec = do
  describe "toGLSL" $ do
    it "compiles constants" $
      toGLSL (setColour (V4 1 0 0 1)) `shouldBe` "#version 410"
