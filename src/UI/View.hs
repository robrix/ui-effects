{-# LANGUAGE FlexibleContexts #-}
module UI.View where

import Control.Action
import Control.Monad.Free
import Control.Comonad.Cofree
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.Functor.Sum
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
renderView = cata $ \ view -> inset margins $ case view of
  Text s -> resizeable (`text` s)
  Label s -> text (pure Nothing) s
  List children -> stack (intersperse (offset spacing (pure ())) children)
  Scroll axis child -> resizeable (\ (Size maxW maxH) ->
    measure child (\ (Size w h) ->
      clip (case axis of
        Just Horizontal -> Size w (fromMaybe h maxH)
        Just Vertical -> Size (fromMaybe w maxW) h
        Nothing -> fromMaybe <$> Size w h <*> Size maxW maxH) child))
  where margins = Size 5 3
        spacing = Point 0 3
        text maxSize = wrap . InL . (`Action` pure) . Draw.Text maxSize
        clip size = wrap . InL . (`Action` pure) . Draw.Clip size
        inset margins = wrap . InR . Inset margins
        offset delta = wrap . InR . Offset delta
        stack :: Foldable t => t (Rendering a ()) -> Rendering a ()
        stack = foldl (>>) (pure ())
        resizeable = wrap . InR . Resizeable
        measure child = wrap . InR . Measure child


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
