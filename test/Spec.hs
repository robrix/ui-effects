import Test.Hspec
import Control.Comonad.Cofree.Cofreer.Spec
import Control.Monad.Free.Freer.Spec
import GL.Shader.Spec
import UI.Layout.Spec

main :: IO ()
main = hspec . parallel $ do
  describe "Control.Comonad.Cofree.Cofreer.Spec" Control.Comonad.Cofree.Cofreer.Spec.spec
  describe "Control.Monad.Free.Freer.Spec" Control.Monad.Free.Freer.Spec.spec
  describe "GL.Shader.Spec" GL.Shader.Spec.spec
  describe "UI.Layout.Spec" UI.Layout.Spec.spec
