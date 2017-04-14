{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, PolyKinds, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Data.Functor.Union where

import qualified Control.Concurrent as CC
import qualified Control.Exception as E
import Control.Monad.Free.Freer
import Control.Monad.IO.Class
import Data.Kind
import qualified Foreign.C.String as C
import Foreign.Ptr
import qualified Foreign.Marshal.Alloc as A
import qualified Foreign.Storable as S

data Union (fs :: [k -> *]) (a :: k) where
  Here :: f a -> Union (f ': fs) a
  There :: Union fs a -> Union (f ': fs) a

type Eff fs = Freer (Union fs)


data Product (fs :: [*]) where
  Nil :: Product '[]
  (:.) :: a -> Product as -> Product (a ': as)


runM :: Monad m => Freer m a -> m a
runM = foldFreer id

foldFreer :: Monad m => (forall x. f x -> m x) -> Freer f a -> m a
foldFreer f = iterFreerA ((>>=) . f)

weaken :: Superset fs gs => Union gs a -> Union fs a
weaken (Here f) = inj f
weaken (There t) = weaken t

weaken1 :: Union fs a -> Union (f ': fs) a
weaken1 = There

strengthen :: Union '[f] a -> f a
strengthen (Here f) = f
strengthen _ = undefined

send :: InUnion fs f => f a -> Eff fs a
send = liftF . inj

sendIO :: InUnion fs IO => IO a -> Eff fs a
sendIO = send

hoistUnion :: (f a -> g a) -> Union (f ': fs) a -> Union (g ': fs) a
hoistUnion f (Here e) = Here (f e)
hoistUnion _ (There t) = There t


type family Superset (fs :: [k]) (gs :: [k]) :: Constraint where
  Superset fs (f ': gs) = (InUnion fs f, Superset fs gs)
  Superset fs '[] = ()


-- Injection and projection

class InUnion (fs :: [k -> *]) (f :: k -> *) where
  inj :: f a -> Union fs a
  prj :: Union fs a -> Maybe (f a)

instance {-# OVERLAPPABLE #-} InUnion (f ': fs) f where
  inj = Here
  prj (Here f) = Just f
  prj _ = Nothing

instance {-# OVERLAPPABLE #-} InUnion fs f => InUnion (g ': fs) f where
  inj = There . inj
  prj (There fs) = prj fs
  prj _ = Nothing


instance Functor f => Functor (Union '[f]) where
  fmap f = Here . fmap f . strengthen

instance (Functor f, Functor (Union (g ': hs))) => Functor (Union (f ': g ': hs)) where
  fmap f (Here e) = Here (fmap f e)
  fmap f (There t) = There (fmap f t)

instance Applicative f => Applicative (Union '[f]) where
  pure = Here . pure
  f <*> a = Here $ strengthen f <*> strengthen a

instance Monad m => Monad (Union '[m]) where
  return = pure
  m >>= f = Here $ strengthen m >>= strengthen . f

instance InUnion fs IO => MonadIO (Freer (Union fs)) where
  liftIO = send

allocaBytes :: InUnion fs IO => Int -> (Ptr a -> Eff fs b) -> Eff fs b
allocaBytes i f = inj (A.allocaBytes i (return . f)) `Then` id

alloca :: forall a b fs. (InUnion fs IO, S.Storable a) => (Ptr a -> Eff fs b) -> Eff fs b
alloca = allocaBytes (sizeOf (undefined :: a))

bracket :: InUnion fs IO => Eff fs a -> (a -> Eff fs b) -> (a -> Eff fs c) -> Eff fs c
bracket before after thing = inj (E.bracket (return before) (return . (>>= after)) (return . (>>= thing))) `Then` id

finally :: InUnion fs IO => Eff fs a -> Eff fs b -> Eff fs a
finally thing ender = inj (E.finally (return thing) (return ender)) `Then` id

throwIO :: (MonadIO m, E.Exception e) => e -> m a
throwIO = liftIO . E.throwIO

peek :: (MonadIO m, S.Storable a) => Ptr a -> m a
peek = liftIO . S.peek

poke :: (MonadIO m, S.Storable a) => Ptr a -> a -> m ()
poke = (liftIO .) . S.poke

pokeElemOff :: (MonadIO m, S.Storable a) => Ptr a -> Int -> a -> m ()
pokeElemOff = ((liftIO .) .) . S.pokeElemOff

sizeOf :: S.Storable a => a -> Int
sizeOf = S.sizeOf

peekCString :: MonadIO m => C.CString -> m String
peekCString = liftIO . C.peekCString

withCString :: InUnion fs IO => String -> (C.CString -> Eff fs a) -> Eff fs a
withCString string f = inj (C.withCString string (return . f)) `Then` id

runInBoundThread :: InUnion fs IO => Eff fs a -> Eff fs a
runInBoundThread action = inj (CC.runInBoundThread (return action)) `Then` id

catch :: (InUnion fs IO, E.Exception e) => Eff fs a -> (e -> Eff fs a) -> Eff fs a
catch act handler = inj (E.catch (return act) (return . handler)) `Then` id
