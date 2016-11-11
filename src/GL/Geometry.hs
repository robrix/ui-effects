{-# LANGUAGE GADTs #-}
module GL.Geometry where

import GL.Scalar

data Mode = Points | Lines | LineLoop | LineStrip | Triangles | TriangleStrip

data Geometry a where
  Geometry :: (Foldable v, GLScalar n) => Mode -> [v n] -> Geometry (v n)
