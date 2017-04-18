{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module UI.View where

import Control.Monad.Free.Freer
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.Functor.Union
import Data.List (intersperse)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (fromMaybe)
import Data.Semigroup (sconcat)
import UI.Drawing hiding (Text)
import qualified UI.Drawing as Draw
import UI.Geometry

-- Datatypes

data ViewF f
  = Text String
  | Label String
  | List [f]
  | Scroll (Maybe Axis) f
  deriving (Eq, Show, Functor)

data Axis = Horizontal | Vertical
  deriving (Eq, Show)

type View = Fix ViewF


renderView :: forall fs a. (InUnion fs (LayoutF a), InUnion fs (DrawingF a), Real a) => View -> Freer (Union fs) (Size a)
renderView = cata $ \ view -> inset (Size 5 3 :: Size a) $ case view of
  Text s -> do
    maxSize <- getMaxSize
    Draw.text maxSize s
  Label s -> Draw.text (pure Nothing) s
  List children -> maybe (pure 0) sconcat (nonEmpty (intersperse spacer children))
  Scroll axis child -> do
    Size maxW maxH <- getMaxSize
    Size w h <- child
    clip (case axis of
      Just Horizontal -> Size w (fromMaybe h maxH)
      Just Vertical -> Size (fromMaybe w maxW) h
      Nothing -> fromMaybe <$> Size w h <*> Size maxW maxH) child
  where spacer = pure (Size 0 3)


-- Smart constructors

text :: String -> View
text = Fix . Text

label :: String -> View
label = Fix . Label

list :: [View] -> View
list = Fix . List

scroll :: Maybe Axis -> View -> View
scroll = (Fix .) . Scroll


-- Instances

instance Show1 ViewF where
  liftShowsPrec sp sl d view = case view of
    Text s -> showsUnaryWith showsPrec "Text" d s
    Label s -> showsUnaryWith showsPrec "Label" d s
    List l -> showsUnaryWith (liftShowsPrec sp sl) "List" d l
    Scroll a f -> showsBinaryWith showsPrec sp "Scroll" d a f
