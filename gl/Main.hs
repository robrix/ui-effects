module Main where

import Control.Monad.Free.Church
import Graphics.UI.GLUT
import UI.View as UI

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "GL"
  displayCallback $= display
  reshapeCallback $= Just reshape
  mainLoop

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

display :: DisplayCallback
display = do
  clear [ ColorBuffer, DepthBuffer ]
  render $ do
    text "hello"
  swapBuffers

render :: View () -> IO ()
render = iterM $ \case
  UI.Text s -> Helvetica12 `renderString` s
  UI.List _ -> pure ()
  UI.Input _ -> pure ()
