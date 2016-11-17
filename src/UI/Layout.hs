{-# LANGUAGE FlexibleInstances, GADTs, StandaloneDeriving #-}
module UI.Layout where

import Control.Applicative
import Control.Comonad.Cofree.Cofreer
import Control.Monad.Free.Freer
import Data.Functor.Algebraic
import Data.Functor.Classes
import Data.Functor.Foldable hiding (unfold)
import Data.Functor.Listable
import Data.Maybe (catMaybes, fromMaybe)
import Data.Semigroup
import UI.Geometry

data Alignment = Leading | Trailing | Full
  deriving (Eq, Ord, Show)

data LayoutF a f where
  Inset :: Size a -> f -> LayoutF a f
  Offset :: Point a -> f -> LayoutF a f
  GetMaxSize :: LayoutF a (Size (Maybe a))
  Align :: Alignment -> f -> LayoutF a f

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
alignLeft = wrap . Align Leading

alignRight :: Layout a b -> Layout a b
alignRight = wrap . Align Trailing


-- Evaluation

measureLayoutSize :: Real a => Layout a (Size a) -> Size a
measureLayoutSize = maybe (Size 0 0) size . fitLayout (pure Nothing)

fitLayoutSize :: Real a => Size (Maybe a) -> Layout a (Size a) -> Maybe (Size a)
fitLayoutSize = (fmap size .) . fitLayout

layoutSizeAlgebra :: Real a => CofreerF (FreerF (LayoutF a) (Size a)) (Alignment, Point a, Size (Maybe a)) (Maybe (Size a)) -> Maybe (Size a)
layoutSizeAlgebra c@(Cofree (_, origin, _) _ _) = size <$> layoutAlgebra (fmap (Rect origin) <$> c)


measureLayout :: Real a => Layout a (Size a) -> Rect a
measureLayout = fromMaybe (Rect (Point 0 0) (Size 0 0)) . fitLayout (pure Nothing)

fitLayout :: Real a => Size (Maybe a) -> Layout a (Size a) -> Maybe (Rect a)
fitLayout = fitLayoutWith layoutAlgebra

fitLayoutAndAnnotate :: Real a => Size (Maybe a) -> Layout a (Size a) -> ALayout a (Size a) (Maybe (Rect a))
fitLayoutAndAnnotate = fitLayoutWith (annotatingBidi layoutAlgebra)

layoutAlgebra :: Real a => Algebra (Fitting (LayoutF a) a) (Maybe (Rect a))
layoutAlgebra (Cofree (_, offset, maxSize) runC layout) = case layout of
  Pure size | maxSize `encloses` size -> Just (Rect offset (fromMaybe <$> size <*> maxSize))
  Free runF layout -> case layout of
    Inset by child -> Rect offset . (2 * by +) . size <$> runC (runF child)
    Offset by child -> Rect offset . (pointSize by +) . size <$> runC (runF child)
    GetMaxSize -> runC (runF maxSize)
    Align _ child -> do
      Rect _ size <- runC (runF child)
      pure $ Rect offset (fromMaybe <$> size <*> maxSize)
  _ -> Nothing
  where maxSize `encloses` size = and (maybe (const True) (>=) <$> maxSize <*> size)


layoutRectanglesAlgebra :: Real a => Algebra (Fitting (LayoutF a) a) [Rect a]
layoutRectanglesAlgebra = wrapAlgebra catMaybes (fmap Just) (collect layoutAlgebra)


type Fitting f a = Bidi f (Size a) (Alignment, Point a, Size (Maybe a))

fitLayoutWith :: Real a => Algebra (Fitting (LayoutF a) a) b -> Size (Maybe a) -> Layout a (Size a) -> b
fitLayoutWith algebra maxSize layout = hylo algebra fittingCoalgebra (Full, Point 0 0, maxSize, layout)

fittingCoalgebra :: Real a => Coalgebra (Fitting (LayoutF a) a) (Alignment, Point a, Size (Maybe a), Layout a (Size a))
fittingCoalgebra (alignment, offset, maxSize, layout) = Cofree (alignment, offset, maxSize) id $ case runFreer layout of
  Pure size -> Pure size
  Free run layout -> case layout of
    Inset by child -> Free id $ Inset by (alignment, addSizeToPoint offset by, subtractSize maxSize (2 * by), run child)
    Offset by child -> Free id $ Offset by (alignment, liftA2 (+) offset by, subtractSize maxSize (pointSize by), run child)
    GetMaxSize -> Free ((,,,) alignment offset maxSize . run) GetMaxSize
    Align alignment child -> Free id $ Align alignment (alignment, offset, Size Nothing (height maxSize), run child)
  where subtractSize maxSize size = liftA2 (-) <$> maxSize <*> (Just <$> size)
        addSizeToPoint point = liftA2 (+) point . sizeExtent


-- Instances

instance Real a => Semigroup (Layout a (Size a)) where
  (<>) = stack

deriving instance Foldable (LayoutF a)

instance (Show a, Show b) => Show (LayoutF a b) where
  showsPrec = liftShowsPrec showsPrec showList

instance Show a => Show1 (LayoutF a) where
  liftShowsPrec sp _ d layout = case layout of
    Inset by child -> showsBinaryWith showsPrec sp "Inset" d by child
    Offset by child -> showsBinaryWith showsPrec sp "Offset" d by child
    GetMaxSize -> showString "GetMaxSize"
    Align alignment child -> showsBinaryWith showsPrec sp "AlignLeft" d alignment child

instance Eq2 LayoutF where
  liftEq2 eqA eqF l1 l2 = case (l1, l2) of
    (Inset s1 c1, Inset s2 c2) -> liftEq eqA s1 s2 && eqF c1 c2
    (Offset p1 c1, Offset p2 c2) -> liftEq eqA p1 p2 && eqF c1 c2
    (GetMaxSize, GetMaxSize) -> True
    (Align a1 c1, Align a2 c2) -> a1 == a2 && eqF c1 c2
    _ -> False

instance Eq a => Eq1 (LayoutF a) where
  liftEq = liftEq2 (==)

instance (Eq a, Eq f) => Eq (LayoutF a f) where
  (==) = liftEq (==)

instance Listable2 LayoutF where
  liftTiers2 t1 t2
    =  liftCons2 (liftTiers t1) t2 Inset
    \/ liftCons2 (liftTiers t1) t2 Offset
    \/ liftCons2 tiers t2 Align

instance Listable a => Listable1 (LayoutF a) where
  liftTiers = liftTiers2 tiers

instance (Listable a, Listable b) => Listable (LayoutF a b) where
  tiers = tiers1

instance Listable Alignment where
  tiers = cons0 Leading
