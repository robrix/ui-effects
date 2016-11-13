import Test.Hspec
import GL.Shader.Spec
import UI.Layout.Spec

main :: IO ()
main = hspec . parallel $ do
  describe "GL.Shader.Spec" GL.Shader.Spec.spec
  describe "UI.Layout.Spec" UI.Layout.Spec.spec
