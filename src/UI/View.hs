{-# LANGUAGE FlexibleContexts #-}
module UI.View where

import Control.Comonad.Cofree
import Data.Functor.Algebraic
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
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

type AView a = Cofree ViewF a


renderView :: Real a => View -> Rendering a ()
renderView = cata $ \ view -> wrapR . Inset (Size 5 3) $ case view of
  Text s -> wrapR (Resizeable (wrapL . flip Draw.Text s))
  Label s -> wrapL (Draw.Text (pure Nothing) s)
  List children -> foldr stack (pure ()) (intersperse (wrapR (Offset (Point 0 3) (pure ()))) children)
  Scroll axis child -> wrapR (Resizeable (\ (Size maxW maxH) ->
    wrapR (Measure child (\ (Size w h) ->
      wrapL (Clip (case axis of
        Just Horizontal -> Size w (fromMaybe h maxH)
        Just Vertical -> Size (fromMaybe w maxW) h
        Nothing -> fromMaybe <$> Size w h <*> Size maxW maxH) child)))))
  where stack each rest = wrapR (Measure each (\ (Size _ h) ->
          each >> wrapR (Offset (Point 0 h) rest)))


-- Smart constructors

text :: String -> View
text = Fix . Text

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
