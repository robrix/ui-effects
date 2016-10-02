{-# LANGUAGE ApplicativeDo, RankNTypes #-}
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad
import Data.Bits
import Data.StateVar hiding (get)
import Data.Time.Clock.POSIX
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
import GL.Setup
import qualified Linear.V2 as Linear
import qualified Linear.V4 as Linear
import Prelude hiding (IO)
import SDL.Raw as SDL
import System.Exit
import UI.Drawing

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

  withWindow name flags (\ window -> withContext window (render window)) `catch` (putStrLn . displayException :: SomeException -> IO ()) `finally` do
    quit
    exitSuccess
  where render window = setup $ \ state -> forever (draw state >> glSwapWindow window)

vertices :: Num n => Shape n -> [Linear.V4 n]
vertices (Rectangle (Linear.V2 ax ay) (Linear.V2 bx by)) =
  [ Linear.V4 ax ay 0 1
  , Linear.V4 ax by 0 1
  , Linear.V4 bx ay 0 1
  , Linear.V4 bx by 0 1
  ]
vertices Circle{} = []

withWindow :: CString -> Word32 -> (Window -> IO a) -> IO a
withWindow name flags = bracket
  (createWindow name SDL_WINDOWPOS_CENTERED SDL_WINDOWPOS_CENTERED (fromInteger w) (fromInteger h) (fromIntegral flags) >>= checkNonNull)
  destroyWindow
  where (w, h) = (1024, 768)

withContext :: Window -> Setup a -> IO a
withContext window setup = bracket
  (glCreateContext window >>= checkNonNull)
  glDeleteContext
  $ \ context -> do
    glMakeCurrent window context >>= check

    glGetString GL_RENDERER >>= peekCString . castPtr >>= putStrLn
    glGetString GL_VERSION >>= peekCString . castPtr >>= putStrLn
    glGetString GL_SHADING_LANGUAGE_VERSION >>= peekCString . castPtr >>= putStrLn

    state <- runSetup setup
    checkGLError
    pure state

setup :: ((GLProgram, GLArray Float) -> IO a) -> Setup a
setup f = do
  _ <- enable DepthTest
  _ <- setDepthFunc Less
  _ <- setClearColour (Linear.V4 0 0 0 (1 :: Float))
  program <- buildProgram [ GL.Setup.Vertex vertexShader, GL.Setup.Fragment fragmentShader ]
  array <- bindArray (vertices shape :: [Linear.V4 Float])
  runIO (f (program, array))
  where shape = Rectangle (Linear.V2 (negate 0.5) (negate 0.5)) (Linear.V2 0.5 0.5)
        vertexShader = lambda "position" $ \ p ->
          set position (uniform "time" * v4 0.3 0.3 0.3 0.3 + get p)
        fragmentShader = set (out "colour") (uniform "time" + v4 0 0 1 (1 :: Float))


draw :: (GLProgram, GLArray Float) -> IO ()
draw (program, array) = do
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

  glUseProgram (unGLProgram program) >> checkGLError

  t <- realToFrac . snd . (properFraction :: POSIXTime -> (Integer, POSIXTime)) <$> getPOSIXTime
  setUniformValue program "time" (Linear.V4 (sin (t * 2 * pi)) (cos (t * negate 2 * pi)) 0 0)

  glBindVertexArray (unGLArray array) >> checkGLError
  glDrawArrays GL_TRIANGLE_STRIP 0 4 >> checkGLError

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
