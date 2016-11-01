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


measureString :: Num a => String -> Size a
measureString s = Size (fromIntegral (length s) * fontW) lineH
  where (fontW, fontH) = (5, 8)
        lineH = fontH + 5

measureStringForWidth :: Real a => a -> String -> Size a
measureStringForWidth maxW s = Size maxW (height line * fromInteger (ceiling (toRational (length s) / (toRational maxW / toRational (width char)))))
  where char = Size 5 8
        line = char + Size 10 5

type Rendering a = Free (Sum (Action Draw.DrawingF) (LayoutF a))

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
        text maxSize = Free . InL . (`Action` Pure) . Draw.Text maxSize
        clip size = Free . InL . (`Action` Pure) . Draw.Clip size
        inset margins = Free . InR . Inset margins
        offset delta = Free . InR . Offset delta
        stack :: Foldable t => t (Rendering a ()) -> Rendering a ()
        stack = foldl (>>) (pure ())
        resizeable = Free . InR . Resizeable
        measure child = Free . InR . Measure child


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
