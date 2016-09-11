{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad
import Data.Bits
import Data.StateVar hiding (get)
import Data.Typeable
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL.Core41
import Graphics.Shader
import GL.Array
import GL.Exception
import GL.Program
import GL.Shader
import Linear.V3
import Prelude hiding (IO)
import SDL.Raw as SDL
import System.Exit

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

  withWindow name flags (\ window -> withContext window setup draw) `catch` (putStrLn . displayException :: SomeException -> IO ()) `finally` do
    quit
    exitSuccess

withWindow :: CString -> Word32 -> (Window -> IO a) -> IO a
withWindow name flags = bracket
  (createWindow name SDL_WINDOWPOS_CENTERED SDL_WINDOWPOS_CENTERED (fromInteger w) (fromInteger h) (fromIntegral flags) >>= checkNonNull)
  destroyWindow
  where (w, h) = (1024, 768)

withContext :: Window -> ((b -> IO a) -> IO a) -> (b -> IO a) -> IO a
withContext window setup draw = bracket
  (glCreateContext window >>= checkNonNull)
  glDeleteContext
  (\ context -> do
    glMakeCurrent window context >>= check

    glGetString GL_RENDERER >>= peekCString . castPtr >>= putStrLn
    glGetString GL_VERSION >>= peekCString . castPtr >>= putStrLn
    glGetString GL_SHADING_LANGUAGE_VERSION >>= peekCString . castPtr >>= putStrLn

    setup $ \ state -> forever (draw state >> checkGLError >> glSwapWindow window))

setup :: ((GLProgram, GLArray Float) -> IO a) -> IO a
setup body = do
  glEnable GL_DEPTH_TEST
  glDepthFunc GL_LESS
  glClearColor 0 0 0 1
  withVertices vertices $ \ array ->
    withBuiltProgram
      [ (GL_VERTEX_SHADER, toGLSL vertexShader)
      , (GL_FRAGMENT_SHADER, toGLSL fragmentShader) ]
      $ \ program -> checkGLError >> body (program, array)
  where vertices =
          [ [ 0, 0.5, 0 ]
          , [ 0.5, negate 0.5, 0 ]
          , [ negate 0.5, negate 0.5, 0 ] ]
        vertexShader = lambda "position" (set position . get)
        fragmentShader = set (out "colour") (V4 1 0 0 1.0)

draw :: (GLProgram, GLArray Float) -> IO ()
draw (program, array) = do
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

  glUseProgram (unGLProgram program) >> checkGLError
  glBindVertexArray (unGLArray array) >> checkGLError
  glDrawArrays GL_TRIANGLES 0 3 >> checkGLError

check :: MonadIO m => CInt -> m ()
check e = when (e < 0) checkSDLError

checkNonNull :: MonadIO m => Ptr a -> m (Ptr a)
checkNonNull p = do
  when (p == nullPtr) checkSDLError
  pure p

checkSDLError :: MonadIO m => m ()
checkSDLError = liftIO $ do
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
