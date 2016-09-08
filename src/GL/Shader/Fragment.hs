{-# LANGUAGE RecordWildCards #-}
module GL.Shader.Fragment where

import Control.Exception
import Control.Monad
import Control.Applicative.Free.Freer
import Data.Foldable (for_)
import Data.List (intercalate)
import Data.Monoid
import Data.Typeable
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL.Core41
import Graphics.GL.Types
import Graphics.Shader.Fragment
import Linear.V4

newtype Shader = Shader { unShader :: GLuint }

newtype Program = Program { unProgram :: GLuint }

newtype ShaderException = ShaderException String
  deriving (Show, Typeable)

newtype ProgramException = ProgramException String
  deriving (Show, Typeable)

toGLSL :: Fragment () -> String
toGLSL shader
  = pragma "version" "410"
  <> main (iter go ("" <$ shader))
  where go (SetColour c rest) = "  gl_FragColor = " <> v4 c <> ";\n" <> rest
        go _ = ""
        v4 (V4 x y z w) = "vec4(" <> intercalate ", " (show <$> [ x, y, z, w ]) <> ")"
        pragma k v = "#" <> k <> " " <> v <> "\n"
        main body = "void main(void) {\n" <> body <> "}"


compile :: String -> IO Shader
compile source = do
  shader <- glCreateShader GL_FRAGMENT_SHADER
  withCString source $ \ source -> do
    alloca $ \ p -> do
      poke p source
      glShaderSource shader 1 p nullPtr
  glCompileShader shader
  checkShader (Shader shader)


link :: [Shader] -> IO Program
link shaders = do
  program <- glCreateProgram
  for_ shaders (glAttachShader program . unShader)
  glLinkProgram program
  for_ shaders (glDetachShader program . unShader)
  checkProgram (Program program)


checkShader :: Shader -> IO Shader
checkShader = fmap Shader . checkStatus glGetShaderiv glGetShaderInfoLog ShaderException GL_COMPILE_STATUS . unShader

checkProgram :: Program -> IO Program
checkProgram = fmap Program . checkStatus glGetProgramiv glGetProgramInfoLog ProgramException GL_LINK_STATUS . unProgram

checkStatus :: Exception e => (GLenum -> GLuint -> Ptr GLint -> IO ()) -> (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()) -> (String -> e) -> GLenum -> GLuint -> IO GLuint
checkStatus get getLog exception status object = do
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
    throw $ exception log
  pure object


instance Exception ShaderException
instance Exception ProgramException
