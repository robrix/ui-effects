module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Free.Church
import Control.Monad
import Data.Bits
import Data.StateVar
import Data.Typeable
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL.Core41
import SDL.Raw as SDL
import System.Exit
import UI.View as UI

main :: IO ()
main = runInBoundThread $ withCString "UI" $ \ name -> do
  SDL.init SDL_INIT_EVERYTHING >>= check
  var SDL_GL_CONTEXT_MAJOR_VERSION $= 4
  var SDL_GL_CONTEXT_MINOR_VERSION $= 1
  var SDL_GL_CONTEXT_PROFILE_MASK  $= SDL_GL_CONTEXT_PROFILE_CORE

  var SDL_GL_RED_SIZE   $= 8
  var SDL_GL_GREEN_SIZE $= 8
  var SDL_GL_BLUE_SIZE  $= 8
  var SDL_GL_ALPHA_SIZE $= 8
  var SDL_GL_DEPTH_SIZE $= 16

  boolVar SDL_GL_DOUBLEBUFFER $= True

  let flags = foldr (.|.) 0 [ SDL_WINDOW_OPENGL
                            , SDL_WINDOW_SHOWN
                            , SDL_WINDOW_RESIZABLE
                            , SDL_WINDOW_ALLOW_HIGHDPI
                            ]

  withWindow name flags (\ window -> withContext window (draw window)) `finally` do
    quit
    exitSuccess

withWindow :: CString -> Word32 -> (Window -> IO a) -> IO a
withWindow name flags = bracket
  (createWindow name SDL_WINDOWPOS_CENTERED SDL_WINDOWPOS_CENTERED (fromInteger w) (fromInteger h) (fromIntegral flags) >>= checkNonNull)
  destroyWindow
  where (w, h) = (1024, 768)

withContext :: Window -> (GLContext -> IO a) -> IO a
withContext window f = bracket
  (glCreateContext window >>= checkNonNull)
  glDeleteContext
  (\ context -> do
    glMakeCurrent window context >>= check
    f context)

draw :: Window -> GLContext -> IO ()
draw window _ = forever $ do
  glClearColor 0 0 0 1
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

  liftIO $ glSwapWindow window

render :: View () -> IO ()
render = iterM $ \case
  UI.Text _ -> pure ()
  UI.List _ -> pure ()
  UI.Input _ -> pure ()

check :: MonadIO m => CInt -> m ()
check e = when (e < 0) checkError

checkNonNull :: MonadIO m => Ptr a -> m (Ptr a)
checkNonNull p = do
  when (p == nullPtr) checkError
  pure p

checkError :: MonadIO m => m ()
checkError = liftIO $ do
  msg <- getError >>= peekCString
  clearError
  when (msg /= "") $ throw $ SDLException msg

var :: GLattr -> StateVar Int
var a = StateVar (get a) (set a)
  where get a = alloca $ \ p -> do
         glGetAttribute a p >>= check
         fromIntegral <$> peek p
        set a i = glSetAttribute a (fromIntegral i) >>= check

boolVar :: GLattr -> StateVar Bool
boolVar = mapStateVar fromEnum toEnum . var

newtype SDLException = SDLException String
  deriving (Show, Typeable)

instance Exception SDLException
