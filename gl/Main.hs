module Main where

import Control.Monad.Free.Church
import Graphics.UI.GLUT
import UI.View as UI

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]

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
  clear [ColorBuffer]
  renderPrimitive Points $
     mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  flush

interpret :: View () -> IO ()
interpret = iterM $ \case
  UI.Text _ -> pure ()
  UI.List _ -> pure ()
  UI.Input _ -> pure ()
