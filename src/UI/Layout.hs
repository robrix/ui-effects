{-# LANGUAGE FlexibleInstances, GADTs #-}
module UI.Layout where

import Control.Applicative
import Control.Comonad.Cofree.Cofreer
import Control.Monad.Free.Freer
import Data.Functor.Algebraic
import Data.Functor.Classes
import Data.Functor.Foldable hiding (unfold)
import Data.Functor.Listable
import Data.Functor.Pretty
import Data.Maybe (catMaybes, fromMaybe)
import Data.Semigroup
import UI.Geometry

data LayoutF a f where
  Inset :: Size a -> f -> LayoutF a f
  Offset :: Point a -> f -> LayoutF a f
  GetMaxSize :: LayoutF a (Size (Maybe a))
  AlignLeft :: f -> LayoutF a f

type Layout a = Freer (LayoutF a)
type ALayout a b = Cofreer (FreerF (LayoutF a) b)


-- Smart constructors

inset :: Size a -> Layout a b -> Layout a b
inset by = wrap . Inset by

offset :: Point a -> Layout a b -> Layout a b
offset by = wrap . Offset by

resizeable :: (Size (Maybe a) -> Layout a b) -> Layout a b
resizeable = (getMaxSize >>=)

getMaxSize :: Layout a (Size (Maybe a))
getMaxSize = liftF GetMaxSize

stack :: Real a => Layout a (Size a) -> Layout a (Size a) -> Layout a (Size a)
stack top bottom = do
  Size w1 h1 <- top
  Size w2 h2 <- offset (Point 0 h1) bottom
  pure $ Size (max w1 w2) h2

alignLeft :: Layout a b -> Layout a b
alignLeft = wrap . AlignLeft


-- Evaluation

measureLayoutSize :: Real a => Layout a (Size a) -> Size a
measureLayoutSize = fromMaybe (Size 0 0) . fitLayoutSize (pure Nothing)

fitLayoutSize :: Real a => Size (Maybe a) -> Layout a (Size a) -> Maybe (Size a)
fitLayoutSize = fitLayoutWith layoutSizeAlgebra

fitLayoutAndAnnotateSize :: Real a => Size (Maybe a) -> Layout a (Size a) -> ALayout a (Size a) (Maybe (Size a))
fitLayoutAndAnnotateSize = fitLayoutWith (annotatingBidi layoutSizeAlgebra)

layoutSizeAlgebra :: Real a => CofreerF (FreerF (LayoutF a) (Size a)) (Point a, Size (Maybe a)) (Maybe (Size a)) -> Maybe (Size a)
layoutSizeAlgebra c@(Cofree (origin, _) _ _) = size <$> layoutAlgebra (fmap (Rect origin) <$> c)


measureLayout :: Real a => Layout a (Size a) -> Rect a
measureLayout = fromMaybe (Rect (Point 0 0) (Size 0 0)) . fitLayout (pure Nothing)

fitLayout :: Real a => Size (Maybe a) -> Layout a (Size a) -> Maybe (Rect a)
fitLayout = fitLayoutWith layoutAlgebra

fitLayoutAndAnnotate :: Real a => Size (Maybe a) -> Layout a (Size a) -> ALayout a (Size a) (Maybe (Rect a))
fitLayoutAndAnnotate = fitLayoutWith (annotatingBidi layoutAlgebra)

layoutAlgebra :: Real a => Algebra (Fitting (LayoutF a) a) (Maybe (Rect a))
layoutAlgebra (Cofree (offset, maxSize) runC layout) = case layout of
  Pure size | maxSize `encloses` size -> Just (Rect offset (fromMaybe <$> size <*> maxSize))
  Free runF layout -> case layout of
    Inset by child -> Rect offset . (2 * by +) . size <$> runC (runF child)
    Offset by child -> Rect offset . (pointSize by +) . size <$> runC (runF child)
    GetMaxSize -> runC (runF maxSize)
    AlignLeft child -> runC (runF child)
  _ -> Nothing
  where maxSize `encloses` size = and (maybe (const True) (>=) <$> maxSize <*> size)


layoutRectanglesAlgebra :: Real a => Algebra (Fitting (LayoutF a) a) [Rect a]
layoutRectanglesAlgebra = wrapAlgebra catMaybes (fmap Just) (collect layoutAlgebra)


type Fitting f a = Bidi f (Size a) (Point a, Size (Maybe a))

fitLayoutWith :: Real a => Algebra (Fitting (LayoutF a) a) b -> Size (Maybe a) -> Layout a (Size a) -> b
fitLayoutWith algebra maxSize layout = hylo algebra fittingCoalgebra (Point 0 0, maxSize, layout)

fittingCoalgebra :: Real a => Coalgebra (Fitting (LayoutF a) a) (Point a, Size (Maybe a), Layout a (Size a))
fittingCoalgebra (offset, maxSize, layout) = Cofree (offset, maxSize) id $ case runFreer layout of
  Pure size -> Pure size
  Free run layout -> case layout of
    Inset by child -> Free id $ Inset by (addSizeToPoint offset by, subtractSize maxSize (2 * by), run child)
    Offset by child -> Free id $ Offset by (liftA2 (+) offset by, subtractSize maxSize (pointSize by), run child)
    GetMaxSize -> Free ((,,) offset maxSize . run) GetMaxSize
    AlignLeft child -> Free id $ AlignLeft (offset, maxSize, run child)
  where subtractSize maxSize size = liftA2 (-) <$> maxSize <*> (Just <$> size)
        addSizeToPoint point = liftA2 (+) point . sizeExtent


-- Instances

instance Real a => Semigroup (Layout a (Size a)) where
  (<>) = stack

instance Foldable (LayoutF a) where
  foldMap f layout = case layout of
    Inset _ child -> f child
    Offset _ child -> f child
    GetMaxSize -> f (pure Nothing)
    AlignLeft child -> f child

instance (Show a, Show b) => Show (LayoutF a b) where
  showsPrec = liftShowsPrec showsPrec showList

instance Show a => Show1 (LayoutF a) where
  liftShowsPrec sp _ d layout = case layout of
    Inset by child -> showsBinaryWith showsPrec sp "Inset" d by child
    Offset by child -> showsBinaryWith showsPrec sp "Offset" d by child
    GetMaxSize -> showString "GetMaxSize"
    AlignLeft child -> showsUnaryWith sp "AlignLeft" d child

instance Eq2 LayoutF where
  liftEq2 eqA eqF l1 l2 = case (l1, l2) of
    (Inset s1 c1, Inset s2 c2) -> liftEq eqA s1 s2 && eqF c1 c2
    (Offset p1 c1, Offset p2 c2) -> liftEq eqA p1 p2 && eqF c1 c2
    (GetMaxSize, GetMaxSize) -> True
    (AlignLeft c1, AlignLeft c2) -> eqF c1 c2
    _ -> False

instance Eq a => Eq1 (LayoutF a) where
  liftEq = liftEq2 (==)

instance (Eq a, Eq f) => Eq (LayoutF a f) where
  (==) = liftEq (==)

instance Pretty2 LayoutF where
  liftPrettyPrec2 p1 pl1 p2 _ d layout = case layout of
    Inset by child -> prettyParen (d > 10) $ text "Inset" </> liftPrettyPrec p1 pl1 11 by </> p2 11 child
    Offset by child -> prettyParen (d > 10) $ text "Offset" </> liftPrettyPrec p1 pl1 11 by </> p2 11 child
    GetMaxSize -> text "GetMaxSize"
    AlignLeft child -> prettyParen (d > 10) $ text "AlignLeft" </> p2 11 child

instance Pretty a => Pretty1 (LayoutF a) where
  liftPrettyPrec = liftPrettyPrec2 prettyPrec prettyList

instance (Pretty a, Pretty b) => Pretty (LayoutF a b) where
  prettyPrec = prettyPrec1

instance Listable2 LayoutF where
  liftTiers2 t1 t2
    =  liftCons2 (liftTiers t1) t2 Inset
    \/ liftCons2 (liftTiers t1) t2 Offset
    \/ liftCons1 t2 AlignLeft

instance Listable a => Listable1 (LayoutF a) where
  liftTiers = liftTiers2 tiers

instance (Listable a, Listable b) => Listable (LayoutF a b) where
  tiers = tiers1
