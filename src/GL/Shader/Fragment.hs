{-# LANGUAGE GADTs, MultiParamTypeClasses, RankNTypes #-}
module GL.Shader.Fragment where

import Control.Exception
import Control.Monad
import Data.Foldable (for_)
import Data.List (intercalate)
import Data.Monoid
import Data.Typeable
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack
import Graphics.GL.Core41
import Graphics.GL.Types
import Graphics.Shader.Fragment
import Linear.V3
import Prelude hiding (IO)
import qualified System.IO as IO

type IO a = HasCallStack => IO.IO a

newtype Shader = Shader { unShader :: GLuint }

newtype Program = Program { unProgram :: GLuint }

newtype VAO = VAO { unVAO :: GLuint }

data GLError
  = InvalidEnum
  | InvalidValue
  | InvalidOperation
  | InvalidFramebufferOperation
  | OutOfMemory
  deriving Show

data GLException = GLException CallStack String
  deriving (Typeable)

toGLSL :: Fragment () -> String
toGLSL shader
  = pragma "version" "410"
  <> "out vec4 fragColour;\n"
  <> main (go shader)
  where go :: Fragment a -> String
        go (SetColour c) = "  fragColour = " <> go c <> ";\n"
        go (V4 x y z w) = "vec4(" <> intercalate ", " (show <$> [ x, y, z, w ]) <> ")"
        go (V2 x y) = "vec2(" <> intercalate ", " (show <$> [ x, y ]) <> ")"
        go (Add a b) = go a <> " + " <> go b
        go (Mul a b) = go a <> " * " <> go b
        go (Sub a b) = go a <> " - " <> go b
        go (Div a b) = go a <> " / " <> go b
        go _ = ""
        pragma k v = "#" <> k <> " " <> v <> "\n"
        main body = "void main(void) {\n" <> body <> "}"


withVertices :: [V3 Float] -> (VAO -> IO a) -> IO a
withVertices vertices body = alloca $ \ p -> do
  glGenBuffers 1 p
  vbo <- peek p
  let bytes = length vertices * 3 * sizeOf (0 :: Float)
  allocaBytes bytes $ \ p -> do
    for_ (zip [0..] vertices) (uncurry (pokeElemOff p))
    glBindBuffer GL_ARRAY_BUFFER vbo
    glBufferData GL_ARRAY_BUFFER (fromIntegral bytes) (castPtr p) GL_STATIC_DRAW
  glGenVertexArrays 1 p
  vao <- peek p
  glBindVertexArray vao
  glEnableVertexAttribArray 0
  glBindBuffer GL_ARRAY_BUFFER vbo
  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr
  body $ VAO vao

withShader :: GLenum -> (Shader -> IO a) -> IO a
withShader shaderType = bracket
  (Shader <$> glCreateShader shaderType)
  (glDeleteShader . unShader)

withCompiledShader :: GLenum -> String -> (Shader -> IO a) -> IO a
withCompiledShader shaderType source body = withShader shaderType $ \ (Shader shader) -> do
    withCString source $ \ source ->
      alloca $ \ p -> do
        poke p source
        glShaderSource shader 1 p nullPtr
    glCompileShader shader
    s <- checkShader (Shader shader)
    body s

withCompiledShaders :: [(GLenum, String)] -> ([Shader] -> IO a) -> IO a
withCompiledShaders sources body = traverse (flip (uncurry withCompiledShader) pure) sources >>= body

withProgram :: (Program -> IO a) -> IO a
withProgram = bracket
  (Program <$> glCreateProgram)
  (glDeleteProgram . unProgram)

withLinkedProgram :: [Shader] -> (Program -> IO a) -> IO a
withLinkedProgram shaders body = withProgram $ \ (Program program) -> do
  for_ shaders (glAttachShader program . unShader)
  checkGLError
  glLinkProgram program
  for_ shaders (glDetachShader program . unShader)
  checkGLError
  p <- checkProgram (Program program)
  body p


withBuiltProgram :: [(GLenum, String)] -> (Program -> IO a) -> IO a
withBuiltProgram sources body = withCompiledShaders sources (`withLinkedProgram` body)


checkShader :: Shader -> IO Shader
checkShader = fmap Shader . checkStatus glGetShaderiv glGetShaderInfoLog GL_COMPILE_STATUS . unShader

checkProgram :: Program -> IO Program
checkProgram = fmap Program . checkStatus glGetProgramiv glGetProgramInfoLog GL_LINK_STATUS . unProgram

checkStatus :: (GLenum -> GLuint -> Ptr GLint -> IO ()) -> (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()) -> GLenum -> GLuint -> IO GLuint
checkStatus get getLog status object = do
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
    throw $ GLException callStack log
  pure object

checkGLError :: IO ()
checkGLError = glGetError >>= \ e -> case e of
  GL_NO_ERROR -> pure ()
  GL_INVALID_ENUM -> throw $ GLException callStack "Invalid enum"
  GL_INVALID_VALUE -> throw $ GLException callStack  "Invalid value"
  GL_INVALID_OPERATION -> throw $ GLException callStack "Invalid operation"
  GL_INVALID_FRAMEBUFFER_OPERATION -> throw $ GLException callStack "Invalid framebuffer operation"
  GL_OUT_OF_MEMORY -> throw $ GLException callStack "Out of memory"
  _ -> throw $ GLException callStack "Unknown exception"


instance Show GLException where
  showsPrec p (GLException s e) = showString "GLException " . showsPrec p e . showChar '\n' . showString (prettyCallStack s)

instance Exception GLException
