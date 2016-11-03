{-# LANGUAGE GADTs #-}
module UI.Drawing
( Shape(..)
, Colour(..)
, DrawingF(..)
, Drawing
, Rendering
, text
, clip
, liftL
, liftR
, wrapL
, wrapR
, boundingRectAlgebra
, module Layout
) where

import Control.Comonad.Cofree.Cofreer
import Control.Monad.Free.Freer
import Data.Functor.Sum
import Data.Maybe (fromMaybe)
import qualified Linear.V2 as Linear
import UI.Layout as Layout
import UI.Font
import UI.Geometry

data Shape a = Rectangle (Linear.V2 a) (Linear.V2 a)

data Colour a = RGBA !a !a !a !a

data DrawingF a f where
  Text :: Size (Maybe a) -> String -> DrawingF a f
  Clip :: Size a -> f -> DrawingF a f
  deriving (Foldable, Functor)

type Drawing a = Freer (DrawingF a)
type Rendering a = Freer (Sum (DrawingF a) (LayoutF a))

text :: Size (Maybe a) -> String -> Drawing a ()
text maxSize = liftF . Text maxSize

clip :: Size a -> Drawing a b -> Drawing a b
clip size = wrap . Clip size

liftL :: Functor l => Freer l a -> Freer (Sum l r) a
liftL (Freer f) = case f of
  Free t r -> wrapL (liftL . t <$> r)
  Pure a -> pure a

liftR :: Functor r => Freer r a -> Freer (Sum l r) a
liftR  (Freer f) = case f of
  Free t r -> wrapR (liftR . t <$> r)
  Pure a -> pure a

wrapL :: l (Freer (Sum l r) a) -> Freer (Sum l r) a
wrapL = wrap . InL

wrapR :: r (Freer (Sum l r) a) -> Freer (Sum l r) a
wrapR = wrap . InR

boundingRectAlgebra :: Real a => CofreerF (FreerF (DrawingF a) (Size a)) (Point a) (Rect a) -> Rect a
boundingRectAlgebra (Cofree origin runC r) = Rect origin $ case r of
  Pure size -> size
  Free runF r -> case runC . runF <$> r of
    Text maxSize s -> fromMaybe <$> maybe measureString measureStringForWidth (width maxSize) s <*> maxSize
    Clip size _ -> size
