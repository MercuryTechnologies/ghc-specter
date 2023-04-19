{-# LANGUAGE GADTs #-}

module GHCSpecter.Control.Free (
  -- * regular
  Free (..),
  liftF,
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
