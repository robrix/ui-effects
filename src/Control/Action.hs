{-# LANGUAGE GADTs #-}
module Control.Action where

-- | An action on a value in some type constructor. Essentially Co-Yoneda. 🎩 Jeremy Gibbons.
data Action f a where
  Action :: f r -> (r -> a) -> Action f a


liftAction :: f a -> Action f a
liftAction a = Action a id

lowerAction :: Functor f => Action f a -> f a
lowerAction (Action m f) = f <$> m

instance Functor (Action f) where
  fmap f (Action functor run) = Action functor (f . run)

instance Applicative f => Applicative (Action f) where
  pure = liftAction . pure
  a <*> b = liftAction (lowerAction a <*> lowerAction b)

instance Monad m => Monad (Action m) where
  return = pure
  a >>= f = liftAction (lowerAction a >>= lowerAction . f)
