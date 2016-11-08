module GL.Shader.Spec where

import Data.List (intercalate)
import GL.Shader
import Linear.V4 as Linear
import Test.Hspec

spec :: Spec
spec = do
  describe "toGLSL" $ do
    it "compiles constants" $
      toGLSL (elaborateShader (v4 1 0 0 1.0 :: Shader (Linear.V4 Float))) `shouldBe` intercalate "\n"
        [ "#version 410"
        , "out vec4 fragColour;"
        , "void main(void) {"
        , "  fragColour = vec4(1.0, 0.0, 0.0, 1.0);"
        , "}"
        ]
