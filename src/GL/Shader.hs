module GL.Shader where

import Graphics.GL.Types

newtype Shader = Shader { unShader :: GLuint }
