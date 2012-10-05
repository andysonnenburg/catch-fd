{-# LANGUAGE CPP #-}
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
module Control.Monad.Catch.Class
       ( MonadThrow (..)
       , MonadCatch (..)
       , mapE
       ) where

import Control.Exception (IOException, ioError)
import qualified Control.Exception as Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS.Lazy as Lazy (RWST (..))
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST (..))
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT (..))
import qualified Control.Monad.Trans.State.Strict as Strict (StateT (..))
import qualified Control.Monad.Trans.Writer.Lazy as Lazy (WriterT (..))
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT (..))

import Data.Monoid (Monoid)

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

#ifdef LANGUAGE_DefaultSignatures
#define MONAD_THROW(T)\
instance MonadThrow e m => MonadThrow e (T m)
#else
#define MONAD_THROW(T)\
instance MonadThrow e m => MonadThrow e (T m) where\
  throw = lift . throw
#endif
MONAD_THROW(IdentityT)
MONAD_THROW(ListT)
MONAD_THROW(MaybeT)
MONAD_THROW(ReaderT r)
#undef MONAD_THROW

#ifdef LANGUAGE_DefaultSignatures
#define MONAD_THROW(C, T)\
instance (C, MonadThrow e m) => MonadThrow e (T m)
#else
#define MONAD_THROW(C, T)\
instance (C, MonadThrow e m) => MonadThrow e (T m) where\
  throw = lift . throw
#endif
MONAD_THROW(Monoid w, Lazy.RWST r w s)
MONAD_THROW(Monoid w, Strict.RWST r w s)
#undef MONAD_THROW

#ifdef LANGUAGE_DefaultSignatures
#define MONAD_THROW(T)\
instance MonadThrow e m => MonadThrow e (T m)
#else
#define MONAD_THROW(T)\
instance MonadThrow e m => MonadThrow e (T m) where\
  throw = lift . throw
#endif
MONAD_THROW(Lazy.StateT s)
MONAD_THROW(Strict.StateT s)
#undef MONAD_THROW

#ifdef LANGUAGE_DefaultSignatures
#define MONAD_THROW(C, T)\
instance (C, MonadThrow e m) => MonadThrow e (T m)
#else
#define MONAD_THROW(C, T)\
instance (C, MonadThrow e m) => MonadThrow e (T m) where\
  throw = lift . throw
#endif
MONAD_THROW(Monoid w, Lazy.WriterT w)
MONAD_THROW(Monoid w, Strict.WriterT w)
#undef MONAD_THROW

instance MonadCatch e m n => MonadCatch e (IdentityT m) (IdentityT n) where
  m `catch` h = IdentityT $ runIdentityT m `catch` (runIdentityT . h)

instance MonadCatch e m n => MonadCatch e (ListT m) (ListT n) where
  m `catch` h = ListT $ runListT m `catch` \ e -> runListT (h e)

instance MonadCatch e m n => MonadCatch e (MaybeT m) (MaybeT n) where
  m `catch` h = MaybeT $ runMaybeT m `catch` (runMaybeT . h)

instance MonadCatch e m n => MonadCatch e (ReaderT r m) (ReaderT r n) where
  m `catch` h =
    ReaderT $ \ r -> runReaderT m r `catch` \ e -> runReaderT (h e) r

instance (Monoid w, MonadCatch e m n) =>
         MonadCatch e (Lazy.RWST r w s m) (Lazy.RWST r w s n) where
  m `catch` h = Lazy.RWST $ \ r s ->
    Lazy.runRWST m r s `catch` \ e -> Lazy.runRWST (h e) r s

instance (Monoid w, MonadCatch e m n) =>
         MonadCatch e (Strict.RWST r w s m) (Strict.RWST r w s n) where
  m `catch` h = Strict.RWST $ \ r s ->
    Strict.runRWST m r s `catch` \ e -> Strict.runRWST (h e) r s

instance MonadCatch e m n =>
         MonadCatch e (Lazy.StateT s m) (Lazy.StateT s n) where
  m `catch` h = Lazy.StateT $ \ s ->
    Lazy.runStateT m s `catch` \ e -> Lazy.runStateT (h e) s

instance MonadCatch e m n =>
         MonadCatch e (Strict.StateT s m) (Strict.StateT s n) where
  m `catch` h = Strict.StateT $ \ s ->
    Strict.runStateT m s `catch` \ e -> Strict.runStateT (h e) s

instance ( Monoid w
         , MonadCatch e m n
         ) => MonadCatch e (Lazy.WriterT w m) (Lazy.WriterT w n) where
  m `catch` h =
    Lazy.WriterT $
    Lazy.runWriterT m `catch` (Lazy.runWriterT . h)

instance ( Monoid w
         , MonadCatch e m n
         ) => MonadCatch e (Strict.WriterT w m) (Strict.WriterT w n) where
  m `catch` h =
    Strict.WriterT $
    Strict.runWriterT m `catch` (Strict.runWriterT . h)
