{-# LANGUAGE RankNTypes #-}
module GL.Exception where

import Control.Exception
import Control.Monad
import Data.Typeable
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack
import Graphics.GL.Core41
import Graphics.GL.Types
import Prelude hiding (IO)
import qualified System.IO as IO

type IO a = HasCallStack => IO.IO a

checkStatus
  :: (GLenum -> GLuint -> Ptr GLint -> IO ())
  -> (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
  -> (String -> GLError)
  -> GLenum
  -> GLuint
  -> IO GLuint
checkStatus get getLog error status object = do
  success <- alloca $ \ p -> do
    get object status p
    peek p
  when (success == GL_FALSE) $ do
    l <- alloca $ \ p -> do
      get object GL_INFO_LOG_LENGTH p
      peek p
    log <- allocaBytes (fromIntegral l) $ \ bytes -> do
      getLog object l nullPtr bytes
      peekCString bytes
    throw $ GLException (error log) callStack
  pure object

checkGLError :: IO ()
checkGLError = glGetError >>= \ e -> case e of
  GL_NO_ERROR -> pure ()
  GL_INVALID_ENUM -> throw $ GLException InvalidEnum callStack
  GL_INVALID_VALUE -> throw $ GLException InvalidValue callStack
  GL_INVALID_OPERATION -> throw $ GLException InvalidOperation callStack
  GL_INVALID_FRAMEBUFFER_OPERATION -> throw $ GLException InvalidFramebufferOperation callStack
  GL_OUT_OF_MEMORY -> throw $ GLException OutOfMemory callStack
  _ -> throw $ GLException (Other "Unknown") callStack


instance Show GLException where
  showsPrec p (GLException e s) = showString "GLException " . showsPrec p e . showChar '\n' . showString (prettyCallStack s)

instance Exception GLException

data GLError
  = InvalidEnum
  | InvalidValue
  | InvalidOperation
  | InvalidFramebufferOperation
  | OutOfMemory
  | Source String String
  | Other String
  deriving Show

data GLException = GLException GLError CallStack
  deriving (Typeable)
