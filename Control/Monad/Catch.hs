{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#endif
{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances #-}
module Control.Monad.Catch
       ( module Exports
       , MonadThrow (..)
       , MonadCatch (..)
       , mapE
       , MonadError
       , WrappedMonadError (..)
       , WrappedMonadCatch (..)
       ) where

import Control.Applicative
import Control.Monad as Exports
import Control.Monad.Catch.Class
import qualified Control.Monad.Error.Class as Error
import Control.Monad.Fix as Exports
import Control.Monad.Trans as Exports

import Prelude (($), (.))

#ifdef LANGUAGE_ConstraintKinds
type MonadError e m = (MonadThrow e m, MonadCatch e m m)
#else
class (MonadThrow e m, MonadCatch e m m) => MonadError e m
instance (MonadThrow e m, MonadCatch e m m) => MonadError e m
#endif

newtype WrappedMonadError m a =
  WrapMonadError { unwrapMonadError :: m a
                 }
instance Functor m => Functor (WrappedMonadError m) where
  fmap f = WrapMonadError . fmap f . unwrapMonadError
  a <$ m = WrapMonadError $ a <$ unwrapMonadError m

instance Applicative m => Applicative (WrappedMonadError m) where
  pure = WrapMonadError . pure
  f <*> a = WrapMonadError $ unwrapMonadError f <*> unwrapMonadError a
  a *> b = WrapMonadError $ unwrapMonadError a *> unwrapMonadError b
  a <* b = WrapMonadError $ unwrapMonadError a <* unwrapMonadError b

instance Monad m => Monad (WrappedMonadError m) where
  return = WrapMonadError . return
  m >>= f = WrapMonadError $ unwrapMonadError m >>= unwrapMonadError . f
  m >> n = WrapMonadError $ unwrapMonadError m >> unwrapMonadError n
  fail = WrapMonadError . fail

instance Error.MonadError e m => MonadThrow e (WrappedMonadError m) where
  throw = WrapMonadError . Error.throwError
instance Error.MonadError e m =>
         MonadCatch e (WrappedMonadError m) (WrappedMonadError m) where
  m `catch` h =
    WrapMonadError $
    unwrapMonadError m `Error.catchError` (unwrapMonadError . h)

newtype WrappedMonadCatch m a =
  WrapMonadCatch { unwrapMonadCatch :: m a
                 }

instance Functor m => Functor (WrappedMonadCatch m) where
  fmap f = WrapMonadCatch . fmap f . unwrapMonadCatch
  a <$ m = WrapMonadCatch $ a <$ unwrapMonadCatch m

instance Applicative m => Applicative (WrappedMonadCatch m) where
  pure = WrapMonadCatch . pure
  f <*> a = WrapMonadCatch $ unwrapMonadCatch f <*> unwrapMonadCatch a
  a *> b = WrapMonadCatch $ unwrapMonadCatch a *> unwrapMonadCatch b
  a <* b = WrapMonadCatch $ unwrapMonadCatch a <* unwrapMonadCatch b

instance Monad m => Monad (WrappedMonadCatch m) where
  return = WrapMonadCatch . return
  m >>= f = WrapMonadCatch $ unwrapMonadCatch m >>= unwrapMonadCatch . f
  m >> n = WrapMonadCatch $ unwrapMonadCatch m >> unwrapMonadCatch n
  fail = WrapMonadCatch . fail

instance MonadCatch e m m => Error.MonadError e (WrappedMonadCatch m) where
  throwError = WrapMonadCatch . throw
  m `catchError` h =
    WrapMonadCatch $ unwrapMonadCatch m `catch` (unwrapMonadCatch . h)
