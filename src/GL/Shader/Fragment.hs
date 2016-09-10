{-# LANGUAGE GADTs, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables #-}
module GL.Shader.Fragment where

import Data.List (intercalate)
import Data.Monoid
import Graphics.Shader.Fragment
import Prelude hiding (IO)

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
