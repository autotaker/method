{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Method where

import Control.Exception (SomeException)
import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.RWS.CPS as CPS
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Select (SelectT)
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.CPS as CPS
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import RIO (MonadReader, MonadUnliftIO, RIO, SimpleGetter, join, throwIO, tryAny, view)

class Monad (Base method) => Method method where
  type Base method :: Type -> Type
  type Args method :: Type
  type Args method = Nil
  type Ret method :: Type
  uncurryMethod :: method -> Args method -> Base method (Ret method)
  default uncurryMethod ::
    (method ~ Base method a, Args method ~ Nil, Ret method ~ a) =>
    method ->
    Args method ->
    Base method (Ret method)
  uncurryMethod method Nil = method
  curryMethod :: (Args method -> Base method (Ret method)) -> method
  default curryMethod ::
    (method ~ Base method a, Args method ~ Nil, Ret method ~ a) =>
    (Args method -> Base method (Ret method)) ->
    method
  curryMethod method' = method' Nil
  joinArgs :: Base method method -> method
  default joinArgs :: method ~ Base method a => Base method method -> method
  joinArgs = join

instance Method (IO a) where
  type Base (IO a) = IO
  type Ret (IO a) = a

instance Method (RIO env a) where
  type Base (RIO env a) = RIO env
  type Ret (RIO env a) = a

instance Method (Identity a) where
  type Base (Identity a) = Identity
  type Ret (Identity a) = a

instance (Monoid w, Monad m) => Method (AccumT w m a) where
  type Base (AccumT w m a) = AccumT w m
  type Ret (AccumT w m a) = a

instance (Monad m) => Method (ContT r m a) where
  type Base (ContT r m a) = ContT r m
  type Ret (ContT r m a) = a

instance (Monad m) => Method (ExceptT e m a) where
  type Base (ExceptT e m a) = ExceptT e m
  type Ret (ExceptT e m a) = a

instance (Monad m) => Method (MaybeT m a) where
  type Base (MaybeT m a) = MaybeT m
  type Ret (MaybeT m a) = a

instance (Monad m) => Method (CPS.RWST r w s m a) where
  type Base (CPS.RWST r w s m a) = CPS.RWST r w s m
  type Ret (CPS.RWST r w s m a) = a

instance (Monad m, Monoid w) => Method (Lazy.RWST r w s m a) where
  type Base (Lazy.RWST r w s m a) = Lazy.RWST r w s m
  type Ret (Lazy.RWST r w s m a) = a

instance (Monad m, Monoid w) => Method (Strict.RWST r w s m a) where
  type Base (Strict.RWST r w s m a) = Strict.RWST r w s m
  type Ret (Strict.RWST r w s m a) = a

instance Monad m => Method (ReaderT r m a) where
  type Base (ReaderT r m a) = ReaderT r m
  type Ret (ReaderT r m a) = a

instance Monad m => Method (SelectT r m a) where
  type Base (SelectT r m a) = SelectT r m
  type Ret (SelectT r m a) = a

instance Monad m => Method (Lazy.StateT s m a) where
  type Base (Lazy.StateT s m a) = Lazy.StateT s m
  type Ret (Lazy.StateT s m a) = a

instance Monad m => Method (Strict.StateT s m a) where
  type Base (Strict.StateT s m a) = Strict.StateT s m
  type Ret (Strict.StateT s m a) = a

instance (Monad m) => Method (CPS.WriterT w m a) where
  type Base (CPS.WriterT w m a) = CPS.WriterT w m
  type Ret (CPS.WriterT w m a) = a

instance (Monad m, Monoid w) => Method (Lazy.WriterT w m a) where
  type Base (Lazy.WriterT w m a) = Lazy.WriterT w m
  type Ret (Lazy.WriterT w m a) = a

instance (Monad m, Monoid w) => Method (Strict.WriterT w m a) where
  type Base (Strict.WriterT w m a) = Strict.WriterT w m
  type Ret (Strict.WriterT w m a) = a

instance Method b => Method (a -> b) where
  type Base (a -> b) = Base b
  type Args (a -> b) = a :* Args b
  type Ret (a -> b) = Ret b
  uncurryMethod method (a :* args) = uncurryMethod (method a) args
  curryMethod method' a = curryMethod (\args -> method' (a :* args))
  joinArgs m v = joinArgs $ m <*> pure v

data Nil = Nil
  deriving (Eq, Ord, Show)

data a :* b = a :* !b
  deriving (Eq, Ord, Show)

infixr 1 :*

decorate ::
  (Method method, MonadUnliftIO (Base method)) =>
  (Args method -> Base method a) ->
  (a -> Either SomeException (Ret method) -> Base method ()) ->
  method ->
  method
decorate before after method = curryMethod $ \args -> do
  a <- before args
  res <- tryAny (uncurryMethod method args)
  case res of
    Left err -> after a res >> throwIO err
    Right v -> after a res >> pure v

decorateBefore ::
  (Method method, MonadUnliftIO (Base method)) =>
  (Args method -> Base method ()) ->
  method ->
  method
decorateBefore before method = curryMethod $ \args -> do
  before args
  uncurryMethod method args

invoke :: (MonadReader env (Base method), Method method) => SimpleGetter env method -> method
invoke getter = joinArgs (view getter)