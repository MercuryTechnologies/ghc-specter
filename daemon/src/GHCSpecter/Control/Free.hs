{-# LANGUAGE GADTs #-}

module GHCSpecter.Control.Free (
  -- * regular
  Free (..),
  liftF,

  -- * indexed
  IxFree (..),
) where

data Free f a = Pure a | Free (f (Free f a))
  deriving (Functor)

instance (Functor f) => Applicative (Free f) where
  pure = Pure
  {-# INLINE pure #-}
  Pure a <*> Pure b = Pure $ a b
  Pure a <*> Free mb = Free $ fmap a <$> mb
  Free ma <*> b = Free $ (<*> b) <$> ma

instance (Functor f) => Monad (Free f) where
  return = pure
  {-# INLINE return #-}
  Pure a >>= f = f a
  Free m >>= f = Free ((>>= f) <$> m)

-- NOTE: a specialized version until we need general monad m that replaces Free f.
liftF :: (Functor f) => f a -> Free f a
liftF fa = Free (fmap pure fa)

class IxFunctor f where
  imap :: (a -> b) -> f i j a -> f i j b

data IxFree f i j a where
  IxPure :: a -> IxFree f i i a
  IxFree :: f i j (IxFree f j k a) -> IxFree f i k a

-- instance IxFunctor f => IxFunctor (IxFree f) where
--   imap :: (a -> b) -> IxFree f i j a -> IxFree f i j b

instance IxFunctor f => Functor (IxFree f i i) where
  fmap :: (a -> b) -> IxFree f i i a -> IxFree f i i b
  fmap f (IxPure x) = IxPure (f x)
  -- fmap f (IxFree mx) = IxFree (fmap (fmap f) mx)
