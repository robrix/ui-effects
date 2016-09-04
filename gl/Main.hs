module Main where

import Control.Monad.Free.Church
import Graphics.UI.GLUT
import UI.View as UI

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "GL"
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do
  clear [ ColorBuffer, DepthBuffer ]
  loadIdentity
  rasterPos $ Vertex2 0 (0 :: GLdouble)
  render $ do
    text "hello"
  swapBuffers

render :: View () -> IO ()
render = iterM $ \case
  UI.Text s -> renderString Helvetica12 s
  UI.List _ -> pure ()
  UI.Input _ -> pure ()
