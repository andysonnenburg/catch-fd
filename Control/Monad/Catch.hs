{-# LANGUAGE CPP #-}
#ifndef __HADDOCK__
{-# LANGUAGE ConstraintKinds #-}
#endif
{-# LANGUAGE
    DefaultSignatures
  , FlexibleInstances
  , FunctionalDependencies
  , MultiParamTypeClasses
  , UndecidableInstances #-}
module Control.Monad.Catch
       ( MonadThrow (..)
       , MonadCatch (..)
       , MonadError
       , mapE
       , WrappedMonadError (..)
       , WrappedMonadCatch (..)
       ) where

import Control.Exception (IOException, ioError)
import qualified Control.Exception as Exception
import Control.Monad
import qualified Control.Monad.Error.Class as Error
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS
import qualified Control.Monad.Trans.State.Lazy as LazyState
import qualified Control.Monad.Trans.State.Strict as StrictState
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter

import Data.Monoid

import Prelude (Either (..), IO, ($), (.), either)

class Monad m => MonadThrow e m | m -> e where
  throw :: e -> m a
  default throw :: (MonadThrow e m, MonadTrans t) => e -> t m a
  throw = lift . throw

class ( MonadThrow e m
      , Monad n
      ) => MonadCatch e m n | m -> e, n e -> m where
  catch :: m a -> (e -> n a) -> n a

type MonadError e m = (MonadThrow e m, MonadCatch e m m)

mapE :: (MonadCatch e m n, MonadThrow e' n) => (e -> e') -> m a -> n a
mapE f m = m `catch` (throw . f)

instance MonadThrow IOException IO where
  throw = ioError
instance MonadCatch IOException IO IO where
  catch = Exception.catch

instance MonadThrow e (Either e) where
  throw = Left
instance MonadCatch e (Either e) (Either e') where
  Left e `catch` h = h e
  Right a `catch` _h = Right a

instance (Error e, Monad m) => MonadThrow e (ErrorT e m) where
  throw = throwError
instance ( Error e
         , Error e'
         , Monad m
         ) => MonadCatch e (ErrorT e m) (ErrorT e' m) where
  m `catch` h = ErrorT $ runErrorT m >>= either (runErrorT . h) (return . Right)

instance MonadThrow e m => MonadThrow e (IdentityT m)
instance MonadCatch e m n => MonadCatch e (IdentityT m) (IdentityT n) where
  m `catch` h = IdentityT $ runIdentityT m `catch` (runIdentityT . h)

instance MonadThrow e m => MonadThrow e (ListT m)
instance MonadCatch e m n => MonadCatch e (ListT m) (ListT n) where
  m `catch` h = ListT $ runListT m `catch` \ e -> runListT (h e)

instance MonadThrow e m => MonadThrow e (MaybeT m)
instance MonadCatch e m n => MonadCatch e (MaybeT m) (MaybeT n) where
  m `catch` h = MaybeT $ runMaybeT m `catch` (runMaybeT . h)

instance MonadThrow e m => MonadThrow e (ReaderT r m)
instance MonadCatch e m n => MonadCatch e (ReaderT r m) (ReaderT r n) where
  m `catch` h =
    ReaderT $ \ r -> runReaderT m r `catch` \ e -> runReaderT (h e) r

instance (Monoid w, MonadThrow e m) => MonadThrow e (LazyRWS.RWST r w s m)
instance (Monoid w, MonadCatch e m n) =>
         MonadCatch e (LazyRWS.RWST r w s m) (LazyRWS.RWST r w s n) where
  m `catch` h = LazyRWS.RWST $ \ r s ->
    LazyRWS.runRWST m r s `catch` \ e -> LazyRWS.runRWST (h e) r s

instance (Monoid w, MonadThrow e m) => MonadThrow e (StrictRWS.RWST r w s m)
instance (Monoid w, MonadCatch e m n) =>
         MonadCatch e (StrictRWS.RWST r w s m) (StrictRWS.RWST r w s n) where
  m `catch` h = StrictRWS.RWST $ \ r s ->
    StrictRWS.runRWST m r s `catch` \ e -> StrictRWS.runRWST (h e) r s

instance MonadThrow e m => MonadThrow e (LazyState.StateT s m)
instance MonadCatch e m n =>
         MonadCatch e (LazyState.StateT s m) (LazyState.StateT s n) where
  m `catch` h = LazyState.StateT $ \ s ->
    LazyState.runStateT m s `catch` \ e -> LazyState.runStateT (h e) s

instance MonadThrow e m => MonadThrow e (StrictState.StateT s m)
instance MonadCatch e m n =>
         MonadCatch e (StrictState.StateT s m) (StrictState.StateT s n) where
  m `catch` h = StrictState.StateT $ \ s ->
    StrictState.runStateT m s `catch` \ e -> StrictState.runStateT (h e) s

instance (Monoid w, MonadThrow e m) => MonadThrow e (LazyWriter.WriterT w m)
instance ( Monoid w
         , MonadCatch e m n
         ) => MonadCatch e (LazyWriter.WriterT w m) (LazyWriter.WriterT w n) where
  m `catch` h =
    LazyWriter.WriterT $
    LazyWriter.runWriterT m `catch` \ e -> LazyWriter.runWriterT (h e)

instance (Monoid w, MonadThrow e m) => MonadThrow e (StrictWriter.WriterT w m)
instance ( Monoid w
         , MonadCatch e m n
         ) => MonadCatch e (StrictWriter.WriterT w m) (StrictWriter.WriterT w n) where
  m `catch` h =
    StrictWriter.WriterT $
    StrictWriter.runWriterT m `catch` \ e -> StrictWriter.runWriterT (h e)

newtype WrappedMonadError m a =
  WrapMonadError { unwrapMonadError :: m a
                 }

instance Monad m => Monad (WrappedMonadError m) where
  return = WrapMonadError . return
  m >>= f = WrapMonadError $ unwrapMonadError m >>= unwrapMonadError . f

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

instance Monad m => Monad (WrappedMonadCatch m) where
  return = WrapMonadCatch . return
  m >>= f = WrapMonadCatch $ unwrapMonadCatch m >>= unwrapMonadCatch . f

instance MonadCatch e m m => Error.MonadError e (WrappedMonadCatch m) where
  throwError = WrapMonadCatch . throw
  m `catchError` h =
    WrapMonadCatch $ unwrapMonadCatch m `catch` (unwrapMonadCatch . h)
