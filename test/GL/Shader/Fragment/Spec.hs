module GL.Shader.Fragment.Spec where

import Data.List (intercalate)
import GL.Shader.Fragment
import Graphics.Shader.Fragment
import Test.Hspec

spec :: Spec
spec = do
  describe "toGLSL" $ do
    it "compiles constants" $
      toGLSL (setColour (V4 1 0 0 1)) `shouldBe` intercalate "\n"
        [ "#version 410"
        , "out vec4 fragColour;"
        , "void main(void) {"
        , "  fragColour = vec4(1.0, 0.0, 0.0, 1.0);"
        , "}"
        ]
