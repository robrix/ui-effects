{-# LANGUAGE GADTs #-}
module UI.Window
( runWindow
, SDLException(..)
) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Bits
import Data.Foldable
import Data.Typeable
import Data.Word
import Foreign.C.String
import Foreign.Ptr
import qualified SDL.Raw as SDL

runWindow :: String -> (IO () -> IO a) -> IO ()
runWindow name draw = runInBoundThread $ withCString name $ \ name -> do
  _ <- SDL.init SDL.SDL_INIT_EVERYTHING >>= checkWhen (< 0)

  SDL.SDL_GL_CONTEXT_MAJOR_VERSION `set` 4
  SDL.SDL_GL_CONTEXT_MINOR_VERSION `set` 1
  SDL.SDL_GL_CONTEXT_PROFILE_MASK `set` SDL.SDL_GL_CONTEXT_PROFILE_CORE

  SDL.SDL_GL_RED_SIZE   `set` 8
  SDL.SDL_GL_GREEN_SIZE `set` 8
  SDL.SDL_GL_BLUE_SIZE  `set` 8
  SDL.SDL_GL_ALPHA_SIZE `set` 8
  SDL.SDL_GL_DEPTH_SIZE `set` 16

  SDL.SDL_GL_DOUBLEBUFFER `set` fromEnum True

  withWindow name flags (\ window ->
    withContext window (const (draw (SDL.glSwapWindow window) >> pure ())))
  `finally`
    SDL.quit
  where flags = foldr (.|.) 0
          [ SDL.SDL_WINDOW_OPENGL
          , SDL.SDL_WINDOW_SHOWN
          , SDL.SDL_WINDOW_RESIZABLE
          , SDL.SDL_WINDOW_ALLOW_HIGHDPI
          ]

ignoreEventsOfTypes :: [Word32] -> IO ()
ignoreEventsOfTypes = traverse_ (\ t -> SDL.eventState t 0 >>= checkWhen (/= 0))

withWindow :: CString -> Word32 -> (SDL.Window -> IO a) -> IO a
withWindow name flags = bracket
  (SDL.createWindow name SDL.SDL_WINDOWPOS_CENTERED SDL.SDL_WINDOWPOS_CENTERED (fromInteger w) (fromInteger h) flags >>= checkNonNull)
  SDL.destroyWindow
  where (w, h) = (1024, 768)

withContext :: SDL.Window -> (SDL.GLContext -> IO a) -> IO a
withContext window = bracket
  (SDL.glCreateContext window >>= checkNonNull)
  SDL.glDeleteContext

checkWhen :: (a -> Bool) -> a -> IO a
checkWhen predicate value = do
  when (predicate value) checkSDLError
  pure value

checkNonNull :: Ptr a -> IO (Ptr a)
checkNonNull = checkWhen (== nullPtr)

checkSDLError :: IO ()
checkSDLError = do
  msg <- SDL.getError >>= peekCString
  SDL.clearError
  when (msg /= "") $ throw $ SDLException msg

set :: SDL.GLattr -> Int -> IO ()
set attribute value = do
  result <- SDL.glSetAttribute attribute (fromIntegral value)
  _ <- checkWhen (< 0) result
  pure ()

newtype SDLException = SDLException String
  deriving (Show, Typeable)

instance Exception SDLException
