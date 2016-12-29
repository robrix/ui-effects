{-# LANGUAGE FlexibleInstances, GADTs, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TypeOperators #-}
module UI.Layout where

import Control.Applicative
import Control.Comonad.Cofree.Cofreer
import Control.Monad.Free.Freer
import Data.Fixed
import Data.Functor.Algebraic
import Data.Functor.Classes
import Data.Functor.Foldable hiding (unfold)
import Data.Functor.Listable
import Data.Maybe (catMaybes, fromMaybe)
import Data.Semigroup
import Data.Typeable
import UI.Geometry

data Alignment = Leading | Trailing | Centre | Full
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

adjacent :: Real a => Layout a (Size a) -> Layout a (Size a) -> Layout a (Size a)
adjacent left right = do
  Size w1 h1 <- left
  Size w2 h2 <- offset (Point w1 0) right
  pure $ Size w2 (max h1 h2)

alignLeft :: Layout a b -> Layout a b
alignLeft = wrap . Align Leading

alignRight :: Layout a b -> Layout a b
alignRight = wrap . Align Trailing

alignCentre :: Layout a b -> Layout a b
alignCentre = wrap . Align Centre

alignFull :: Layout a b -> Layout a b
alignFull = wrap . Align Full

align :: Alignment -> Layout a b -> Layout a b
align = (wrap .) . Align


-- Evaluation

measureLayoutSize :: Real a => Layout a (Size a) -> Size a
measureLayoutSize = maybe (Size 0 0) size . fitLayout (pure Nothing)

fitLayoutSize :: Real a => Size (Maybe a) -> Layout a (Size a) -> Maybe (Size a)
fitLayoutSize = (fmap size .) . fitLayout


measureLayout :: Real a => Layout a (Size a) -> Rect a
measureLayout = fromMaybe (Rect (Point 0 0) (Size 0 0)) . fitLayout (pure Nothing)

fitLayout :: Real a => Size (Maybe a) -> Layout a (Size a) -> Maybe (Rect a)
fitLayout = fitLayoutWith layoutAlgebra

layoutAlgebra :: Real a => Algebra (Fitting (LayoutF a) a) (Maybe (Rect a))
layoutAlgebra (Bidi FittingState{..} layout) = case layout of
  Pure size | maxSize `encloses` size -> Just $ case alignment of
    Leading -> Rect origin minSize
    Trailing -> Rect origin { x = x origin + widthDiff } minSize
    Centre -> Rect origin { x = x origin + fromIntegral (widthDiff `div'` 2 :: Int) } minSize
    Full -> Rect origin fullSize
    where minSize = fullSize { width = width size }
          fullSize = fromMaybe <$> size <*> maxSize
          widthDiff = maybe 0 (+ negate (width size)) (width maxSize)
  Free runF layout -> case layout of
    Inset by child -> Rect origin . (2 * by +) . size <$> runF child
    Offset by child -> Rect origin . (pointSize by +) . size <$> runF child
    GetMaxSize -> runF maxSize
    Align _ child -> do
      Rect _ size <- runF child
      pure $ Rect origin (fromMaybe <$> size <*> maxSize)
  _ -> Nothing
  where maxSize `encloses` size = and (maybe (const True) (>=) <$> maxSize <*> size)

layoutBackgroundRectAlgebra :: Real a => Algebra (Fitting (LayoutF a) a) (Either (Rect a) (Rect a))
layoutBackgroundRectAlgebra = wrapAlgebra (Left . fromMaybe (Rect (Point 0 0) 0)) (either Just Just) layoutAlgebra


layoutRectanglesAlgebra :: Real a => Algebra (Fitting (LayoutF a) a) [Rect a]
layoutRectanglesAlgebra = wrapAlgebra catMaybes (fmap Just) (collect layoutAlgebra)


type Fitting f a = Bidi (FreerF f (Size a)) (FittingState a)

data FittingState a = FittingState { alignment :: !Alignment, origin :: !(Point a), maxSize :: !(Size (Maybe a)) }
  deriving (Eq, Show)

fitLayoutWith :: Real a => Algebra (Fitting (LayoutF a) a) b -> Size (Maybe a) -> Layout a (Size a) -> b
fitLayoutWith algebra maxSize layout = hylo algebra layoutCoalgebra (Bidi (FittingState Full (Point 0 0) maxSize) (runFreer layout))

layoutCoalgebra :: Real a => Coalgebra (Fitting (LayoutF a) a) (Fitting (LayoutF a) a (Layout a (Size a)))
layoutCoalgebra = liftBidiCoalgebra layoutFCoalgebra

layoutFCoalgebra :: Real a => CoalgebraFragment (LayoutF a) (FittingState a) (Size a)
layoutFCoalgebra state@FittingState{..} run layoutF = case layoutF of
  Inset by child -> wrapState (FittingState alignment (addSizeToPoint origin by) (subtractSize maxSize (2 * by))) $ Inset by child
  Offset by child -> wrapState (FittingState alignment (liftA2 (+) origin by) (subtractSize maxSize (pointSize by))) $ Offset by child
  GetMaxSize -> wrapState state GetMaxSize
  Align alignment child -> wrapState (state { alignment = alignment }) $ Align alignment child
  where wrapState state = Free (run state)
        subtractSize maxSize size = liftA2 (-) <$> maxSize <*> (Just <$> size)
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

instance (Typeable a, Typeable b, Listable a, Listable b) => Listable (LayoutF a b) where
  tiers = case eqT :: Maybe (b :~: Size (Maybe a)) of
    Just Refl -> tiers1 \/ cons0 GetMaxSize
    Nothing   -> tiers1

instance Listable Alignment where
  tiers
    =  cons0 Leading
    \/ cons0 Trailing
    \/ cons0 Centre
    \/ cons0 Full
