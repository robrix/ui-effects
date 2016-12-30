{-# LANGUAGE GADTs, StandaloneDeriving #-}
module GL.Geometry where

import GL.Array
import GL.Scalar

data Mode = Points | Lines | LineLoop | LineStrip | Triangles | TriangleStrip
  deriving Show

data Geometry a where
  Geometry :: (Foldable v, GLScalar n) => Mode -> [v n] -> Geometry (v n)

data ArrayRange = ArrayRange { mode :: Mode, firstVertexIndex :: Int, vertexCount :: Int }
  deriving Show

data GeometryArray n = GeometryArray { geometryRanges :: [ArrayRange], geometryArray :: GLArray n }
  deriving Show


-- Instances

deriving instance Foldable Geometry
deriving instance Show a => Show (Geometry a)
