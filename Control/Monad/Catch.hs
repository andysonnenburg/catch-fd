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
import Control.Monad.Cont.Class
import qualified Control.Monad.Error.Class as Error
import Control.Monad.Fix as Exports
import Control.Monad.RWS.Class
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

instance Alternative m => Alternative (WrappedMonadError m) where
  empty = WrapMonadError empty
  m <|> n = WrapMonadError $ unwrapMonadError m <|> unwrapMonadError n
  some = WrapMonadError . some . unwrapMonadError
  many = WrapMonadError . many . unwrapMonadError

instance Monad m => Monad (WrappedMonadError m) where
  return = WrapMonadError . return
  m >>= f = WrapMonadError $ unwrapMonadError m >>= unwrapMonadError . f
  m >> n = WrapMonadError $ unwrapMonadError m >> unwrapMonadError n
  fail = WrapMonadError . fail

instance MonadTrans WrappedMonadError where
  lift = WrapMonadError

instance MonadIO m => MonadIO (WrappedMonadError m) where
  liftIO = WrapMonadError . liftIO

instance Error.MonadError e m => MonadThrow e (WrappedMonadError m) where
  throw = WrapMonadError . Error.throwError
instance Error.MonadError e m =>
         MonadCatch e (WrappedMonadError m) (WrappedMonadError m) where
  m `catch` h =
    WrapMonadError $
    unwrapMonadError m `Error.catchError` (unwrapMonadError . h)

instance MonadCont m => MonadCont (WrappedMonadError m) where
  callCC f =
    WrapMonadError $ callCC $ \ c -> unwrapMonadError (f (WrapMonadError . c))

instance Error.MonadError e m => Error.MonadError e (WrappedMonadError m) where
  throwError = WrapMonadError . Error.throwError
  m `catchError` h =
    WrapMonadError $
    unwrapMonadError m `Error.catchError` (unwrapMonadError . h)

instance MonadRWS r w s m => MonadRWS r w s (WrappedMonadError m)

instance MonadReader r m => MonadReader r (WrappedMonadError m) where
  ask = WrapMonadError ask
  local f = WrapMonadError . local f . unwrapMonadError
  reader = WrapMonadError . reader

instance MonadState s m => MonadState s (WrappedMonadError m) where
  get = WrapMonadError get
  put = WrapMonadError . put
  state = WrapMonadError . state

instance MonadWriter w m => MonadWriter w (WrappedMonadError m) where
  writer = WrapMonadError . writer
  tell = WrapMonadError . tell
  listen = WrapMonadError . listen . unwrapMonadError
  pass = WrapMonadError . pass . unwrapMonadError

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

instance Alternative m => Alternative (WrappedMonadCatch m) where
  empty = WrapMonadCatch empty
  m <|> n = WrapMonadCatch $ unwrapMonadCatch m <|> unwrapMonadCatch n
  some = WrapMonadCatch . some . unwrapMonadCatch
  many = WrapMonadCatch . many . unwrapMonadCatch

instance Monad m => Monad (WrappedMonadCatch m) where
  return = WrapMonadCatch . return
  m >>= f = WrapMonadCatch $ unwrapMonadCatch m >>= unwrapMonadCatch . f
  m >> n = WrapMonadCatch $ unwrapMonadCatch m >> unwrapMonadCatch n
  fail = WrapMonadCatch . fail

instance MonadTrans WrappedMonadCatch where
  lift = WrapMonadCatch

instance MonadIO m => MonadIO (WrappedMonadCatch m) where
  liftIO = WrapMonadCatch . liftIO

instance MonadThrow e m => MonadThrow e (WrappedMonadCatch m) where
  throw = WrapMonadCatch . throw
instance MonadCatch e m n =>
         MonadCatch e (WrappedMonadCatch m) (WrappedMonadCatch n) where
  m `catch` h =
    WrapMonadCatch $ unwrapMonadCatch m `catch` (unwrapMonadCatch . h)

instance MonadCont m => MonadCont (WrappedMonadCatch m) where
  callCC f =
    WrapMonadCatch $ callCC $ \ c -> unwrapMonadCatch (f (WrapMonadCatch . c))

instance MonadCatch e m m => Error.MonadError e (WrappedMonadCatch m) where
  throwError = WrapMonadCatch . throw
  m `catchError` h =
    WrapMonadCatch $ unwrapMonadCatch m `catch` (unwrapMonadCatch . h)

instance MonadRWS r w s m => MonadRWS r w s (WrappedMonadCatch m)

instance MonadReader r m => MonadReader r (WrappedMonadCatch m) where
  ask = WrapMonadCatch ask
  local f = WrapMonadCatch . local f . unwrapMonadCatch
  reader = WrapMonadCatch . reader

instance MonadState s m => MonadState s (WrappedMonadCatch m) where
  get = WrapMonadCatch get
  put = WrapMonadCatch . put
  state = WrapMonadCatch . state

instance MonadWriter w m => MonadWriter w (WrappedMonadCatch m) where
  writer = WrapMonadCatch . writer
  tell = WrapMonadCatch . tell
  listen = WrapMonadCatch . listen . unwrapMonadCatch
  pass = WrapMonadCatch . pass . unwrapMonadCatch
