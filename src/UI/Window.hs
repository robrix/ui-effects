{-# LANGUAGE FlexibleContexts, GADTs #-}
module UI.Window
( runWindow
, SDLException(..)
) where

import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Data.Foldable
import Data.Functor.Union
import Data.Typeable
import Data.Word
import qualified Foreign.C.String as C
import Foreign.Ptr
import qualified SDL.Raw as SDL

runWindow :: InUnion fs IO => String -> (Eff fs () -> Eff fs a) -> Eff fs ()
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

  ignoreEventsOfTypes
    [ SDL.SDL_FINGERMOTION
    , SDL.SDL_FINGERUP
    , SDL.SDL_FINGERDOWN ]

  withWindow name flags (\ window ->
    withContext window (const (draw (SDL.glSwapWindow window) >> pure ())))
  `finally`
    SDL.quit
  where flags = foldr (.|.) 0
          [ SDL.SDL_WINDOW_OPENGL
          , SDL.SDL_WINDOW_SHOWN
          , SDL.SDL_WINDOW_RESIZABLE
          , SDL.SDL_WINDOW_ALLOW_HIGHDPI ]

ignoreEventsOfTypes :: MonadIO m => [Word32] -> m ()
ignoreEventsOfTypes = traverse_ (\ t -> SDL.eventState t 0 >>= checkWhen (/= 0))

withWindow :: InUnion fs IO => C.CString -> Word32 -> (SDL.Window -> Eff fs a) -> Eff fs a
withWindow name flags = bracket
  (SDL.createWindow name SDL.SDL_WINDOWPOS_CENTERED SDL.SDL_WINDOWPOS_CENTERED (fromInteger w) (fromInteger h) flags >>= checkNonNull)
  SDL.destroyWindow
  where (w, h) = (1024, 768)

withContext :: InUnion fs IO => SDL.Window -> (SDL.GLContext -> Eff fs a) -> Eff fs a
withContext window = bracket
  (SDL.glCreateContext window >>= checkNonNull)
  SDL.glDeleteContext

checkWhen :: MonadIO m => (a -> Bool) -> a -> m a
checkWhen predicate value = do
  when (predicate value) checkSDLError
  pure value

checkNonNull :: MonadIO m => Ptr a -> m (Ptr a)
checkNonNull = checkWhen (== nullPtr)

checkSDLError :: MonadIO m => m ()
checkSDLError = do
  msg <- SDL.getError >>= peekCString
  SDL.clearError
  when (msg /= "") $ E.throw $ SDLException msg

set :: MonadIO m => SDL.GLattr -> Int -> m ()
set attribute value = do
  result <- SDL.glSetAttribute attribute (fromIntegral value)
  _ <- checkWhen (< 0) result
  pure ()

newtype SDLException = SDLException String
  deriving (Show, Typeable)

instance E.Exception SDLException
