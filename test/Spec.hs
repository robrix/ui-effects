import Test.Hspec
import GL.Shader.Spec

main :: IO ()
main = hspec . parallel $ do
  describe "GL.Shader.Spec" $ GL.Shader.Spec.spec
