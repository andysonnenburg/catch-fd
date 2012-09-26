{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#endif
#ifdef LANGUAGE_DefaultSignatures
{-# LANGUAGE DefaultSignatures #-}
#endif
{-# LANGUAGE
    FlexibleInstances
  , FunctionalDependencies
  , MultiParamTypeClasses
  , UndecidableInstances #-}
{- |
License: BSD-style (see the file LICENSE)
Maintainer: Andy Sonnenburg <andy22286@gmail.com>
Stability: experimental
Portability: non-portable

[Computation type:]
Computations which may fail or throw exceptions; and computations which may
catch failures and thrown exceptions.

[Binding strategy:]
Failure records information about the cause/location of the failure.  Failure
values bypass the bound function; other values are used as inputs to the bound
function (same as @'Control.Monad.Error.Class.MonadError'@).

[Useful for:]
Building computations from sequences of functions that may fail; and using
exception handling to structure error handling.  The handler may or may not
throw an exception, which does not have to be of the same type as the original
thrown exception (see @'mapE'@).

[Zero and plus:]
Zero is represented by an empty error, and the plus operation executes its
second argument if the first fails (same as
@'Control.Monad.Error.Class.MonadError'@).

[Example type:]
@'Either' 'String' a@

The Throw and Catch monads.
-}
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

{- |
The strategy of combining computations that can throw exceptions.

Is parameterized over the type of error information and the monad type
constructor.  It is common to use @'Either' 'String'@.  In some cases you will
have to define an instance of @'MonadThrow'@, though rarely a definition of
@'throw'@
-}
class Monad m => MonadThrow e m | m -> e where
  {- |
  Is used within a monadic computation to begin exception processing.  If
  @('MonadThrow' e n, 'MonadTrans' t) => t n ~ m@, then @'throw' = 'lift' '.'
  'throw'@ is the default definition.
  -}
  throw :: e -> m a
#ifdef LANGUAGE_DefaultSignatures
  default throw :: (MonadThrow e m, MonadTrans t) => e -> t m a
  throw = lift . throw
#endif

{- |
The strategy of combining computations that can handle thrown exceptions,
as well as throwing exceptions in the original computation.

Is parameterized over the type of error information and the original monad type
constructor, as well as the handler monad type constructor.  The handler monad
type constructor commonly differs from the original monad type constructor due
to a change in the type of the error information.
-}
class (MonadThrow e m, Monad n) => MonadCatch e m n | n e -> m where
  {- |
  A handler function to handle thrown values and return to normal execution.
  A common idiom is:

  > do { action1; action2; action3 } `catch` handler

  where the @action@ functions can call 'throw'.
  Note that @handler@ and the do-block must have the same return type.
  -}
  catch :: m a -> (e -> n a) -> n a

#ifdef LANGUAGE_ConstraintKinds
type MonadError e m = (MonadThrow e m, MonadCatch e m m)
#else
class (MonadThrow e m, MonadCatch e m m) => MonadError e m
instance (MonadThrow e m, MonadCatch e m m) => MonadError e m
#endif

-- | Map the thrown value using the given function
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
#ifndef LANGUAGE_DefaultSignatures
  where throw = lift . throw
#endif
instance MonadCatch e m n => MonadCatch e (IdentityT m) (IdentityT n) where
  m `catch` h = IdentityT $ runIdentityT m `catch` (runIdentityT . h)

instance MonadThrow e m => MonadThrow e (ListT m)
#ifndef LANGUAGE_DefaultSignatures
  where throw = lift . throw
#endif
instance MonadCatch e m n => MonadCatch e (ListT m) (ListT n) where
  m `catch` h = ListT $ runListT m `catch` \ e -> runListT (h e)

instance MonadThrow e m => MonadThrow e (MaybeT m)
#ifndef LANGUAGE_DefaultSignatures
  where throw = lift . throw
#endif
instance MonadCatch e m n => MonadCatch e (MaybeT m) (MaybeT n) where
  m `catch` h = MaybeT $ runMaybeT m `catch` (runMaybeT . h)

instance MonadThrow e m => MonadThrow e (ReaderT r m)
#ifndef LANGUAGE_DefaultSignatures
  where throw = lift . throw
#endif
instance MonadCatch e m n => MonadCatch e (ReaderT r m) (ReaderT r n) where
  m `catch` h =
    ReaderT $ \ r -> runReaderT m r `catch` \ e -> runReaderT (h e) r

instance (Monoid w, MonadThrow e m) => MonadThrow e (LazyRWS.RWST r w s m)
#ifndef LANGUAGE_DefaultSignatures
  where throw = lift . throw
#endif
instance (Monoid w, MonadCatch e m n) =>
         MonadCatch e (LazyRWS.RWST r w s m) (LazyRWS.RWST r w s n) where
  m `catch` h = LazyRWS.RWST $ \ r s ->
    LazyRWS.runRWST m r s `catch` \ e -> LazyRWS.runRWST (h e) r s

instance (Monoid w, MonadThrow e m) => MonadThrow e (StrictRWS.RWST r w s m)
#ifndef LANGUAGE_DefaultSignatures
  where throw = lift . throw
#endif
instance (Monoid w, MonadCatch e m n) =>
         MonadCatch e (StrictRWS.RWST r w s m) (StrictRWS.RWST r w s n) where
  m `catch` h = StrictRWS.RWST $ \ r s ->
    StrictRWS.runRWST m r s `catch` \ e -> StrictRWS.runRWST (h e) r s

instance MonadThrow e m => MonadThrow e (LazyState.StateT s m)
#ifndef LANGUAGE_DefaultSignatures
  where throw = lift . throw
#endif
instance MonadCatch e m n =>
         MonadCatch e (LazyState.StateT s m) (LazyState.StateT s n) where
  m `catch` h = LazyState.StateT $ \ s ->
    LazyState.runStateT m s `catch` \ e -> LazyState.runStateT (h e) s

instance MonadThrow e m => MonadThrow e (StrictState.StateT s m)
#ifndef LANGUAGE_DefaultSignatures
  where throw = lift . throw
#endif
instance MonadCatch e m n =>
         MonadCatch e (StrictState.StateT s m) (StrictState.StateT s n) where
  m `catch` h = StrictState.StateT $ \ s ->
    StrictState.runStateT m s `catch` \ e -> StrictState.runStateT (h e) s

instance (Monoid w, MonadThrow e m) => MonadThrow e (LazyWriter.WriterT w m)
#ifndef LANGUAGE_DefaultSignatures
  where throw = lift . throw
#endif
instance
  ( Monoid w
  , MonadCatch e m n
  ) => MonadCatch e (LazyWriter.WriterT w m) (LazyWriter.WriterT w n) where
  m `catch` h =
    LazyWriter.WriterT $
    LazyWriter.runWriterT m `catch` (LazyWriter.runWriterT . h)

instance (Monoid w, MonadThrow e m) => MonadThrow e (StrictWriter.WriterT w m)
#ifndef LANGUAGE_DefaultSignatures
  where throw = lift . throw
#endif
instance
  ( Monoid w
  , MonadCatch e m n
  ) => MonadCatch e (StrictWriter.WriterT w m) (StrictWriter.WriterT w n) where
  m `catch` h =
    StrictWriter.WriterT $
    StrictWriter.runWriterT m `catch` (StrictWriter.runWriterT . h)

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
