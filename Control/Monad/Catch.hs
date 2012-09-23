{-# LANGUAGE
    ConstraintKinds
  , DefaultSignatures
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

import Control.Exception (IOException)
import Control.Monad.Error hiding (MonadError)
import qualified Control.Monad.Error.Class as Error
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict

import Data.Monoid

class Monad m => MonadThrow e m | m -> e where
  throw :: e -> m a
  default throw :: (MonadThrow e m, MonadTrans t) => e -> t m a
  throw = lift . throw

class ( MonadThrow e m
      , Monad n
      ) => MonadCatch e m n | m -> e, n e -> m where
  catch :: m a -> (e -> n a) -> n a

type MonadError e m = (MonadThrow e m, MonadCatch e m m)

mapE :: (MonadThrow e' n, MonadCatch e m n) => (e -> e') -> m a -> n a
mapE f m = m `catch` (throw . f)

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
  m `catch` h = ReaderT $ \ r -> runReaderT m r `catch` \ e -> runReaderT (h e) r

instance (Monoid w, MonadThrow e m) => MonadThrow e (Lazy.RWST r w s m)
instance (Monoid w, MonadCatch e m n) =>
         MonadCatch e (Lazy.RWST r w s m) (Lazy.RWST r w s n) where
  m `catch` h =
    Lazy.RWST $ \ r s -> Lazy.runRWST m r s `catch` \ e -> Lazy.runRWST (h e) r s

instance (Monoid w, MonadThrow e m) => MonadThrow e (Strict.RWST r w s m)
instance (Monoid w, MonadCatch e m n) =>
         MonadCatch e (Strict.RWST r w s m) (Strict.RWST r w s n) where
  m `catch` h = Strict.RWST $ \ r s ->
    Strict.runRWST m r s `catch` \ e -> Strict.runRWST (h e) r s

instance MonadThrow e m => MonadThrow e (Lazy.StateT s m)
instance MonadCatch e m n =>
         MonadCatch e (Lazy.StateT s m) (Lazy.StateT s n) where
  m `catch` h = Lazy.StateT $ \ s ->
    Lazy.runStateT m s `catch` \ e -> Lazy.runStateT (h e) s

instance MonadThrow e m => MonadThrow e (Strict.StateT s m)
instance MonadCatch e m n =>
         MonadCatch e (Strict.StateT s m) (Strict.StateT s n) where
  m `catch` h = Strict.StateT $ \ s ->
    Strict.runStateT m s `catch` \ e -> Strict.runStateT (h e) s

instance (Monoid w, MonadThrow e m) => MonadThrow e (Lazy.WriterT w m)
instance ( Monoid w
         , MonadCatch e m n
         ) => MonadCatch e (Lazy.WriterT w m) (Lazy.WriterT w n) where
  m `catch` h =
    Lazy.WriterT $ Lazy.runWriterT m `catch` \ e -> Lazy.runWriterT (h e)

instance ( Monoid w
         , MonadCatch e m n
         ) => MonadCatch e (Strict.WriterT w m) (Strict.WriterT w n) where
  m `catch` h =
    Strict.WriterT $ Strict.runWriterT m `catch` \ e -> Strict.runWriterT (h e)

instance (Monoid w, MonadThrow e m) => MonadThrow e (Strict.WriterT w m)

instance MonadThrow e (Either e) where
  throw = Left
instance MonadCatch e (Either e) (Either e') where
  Left e `catch` h = h e
  Right a `catch` _h = Right a

instance MonadThrow IOException IO where
  throw = throwError
instance MonadCatch IOException IO IO where
  catch = catchError

newtype WrappedMonadError m a =
  WrapMonadError { unwrapMonadError :: m a
                 }

instance Monad m => Monad (WrappedMonadError m) where
  return = WrapMonadError . return
  m >>= f = WrapMonadError $ unwrapMonadError m >>= unwrapMonadError . f

instance Error.MonadError e m => MonadThrow e (WrappedMonadError m) where
  throw = WrapMonadError . throwError
instance Error.MonadError e m =>
         MonadCatch e (WrappedMonadError m) (WrappedMonadError m) where
  m `catch` h =
    WrapMonadError $ unwrapMonadError m `catchError` (unwrapMonadError . h)

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
