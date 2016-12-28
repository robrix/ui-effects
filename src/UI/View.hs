{-# LANGUAGE FlexibleContexts #-}
module UI.View where

import Data.Functor.Algebraic
import Data.Functor.Classes
import Data.Functor.Foldable
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


renderView :: Real a => View -> Rendering a (Size a)
renderView = cata $ \ view -> wrapR . Inset (Size 5 3) $ case view of
  Text s -> do
    maxSize <- liftFR GetMaxSize
    liftFL (Draw.Text maxSize s)
  Label s -> wrapL (Background (rgba 1 0 0 1) (liftFL (Draw.Text (pure Nothing) s)))
  List children -> wrapL (Background (rgba 0 1 0 1) (maybe (pure 0) sconcat (nonEmpty (intersperse spacer children))))
  Scroll axis child -> do
    Size maxW maxH <- liftFR GetMaxSize
    Size w h <- child
    wrapL (Clip (case axis of
      Just Horizontal -> Size w (fromMaybe h maxH)
      Just Vertical -> Size (fromMaybe w maxW) h
      Nothing -> fromMaybe <$> Size w h <*> Size maxW maxH) child)
  where spacer = pure (Size 5 3)


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
