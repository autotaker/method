{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module : Control.Method
-- License: BSD-3
-- Maintainer: autotaker@gmail.com
-- Stability: experimental
module Control.Method
  ( -- * Usage
    -- $usage

    -- ** Dependency Injection
    -- $di

    -- ** Decorating methods
    -- $decorate

    -- * References
    Method (..),
    TupleLike (..),
    decorate,
    decorate_,
    decorateBefore_,
    invoke,
    liftJoin,
  )
where

import Control.Exception (SomeException)
import Control.Method.Internal
  ( Nil (Nil),
    TupleLike (AsTuple, fromTuple, toTuple),
    type (:*) ((:*)),
  )
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
import RIO (MonadReader, MonadUnliftIO, RIO, ST, SimpleGetter, throwIO, tryAny, view)

-- $usage
-- This module provides dependency injection and decoration
-- for monadic functions (called methods).

-- $di
--
-- For example, assume that we are implementing signin function,
-- which checks user's password.
--
-- First, let's create an interface to access database.
--
-- @
-- type UserRepository env = UserRepository {
--   findById :: UserId -> 'RIO' env (Maybe User)
--   create :: User -> 'RIO' env UserId
-- }
-- @
--
-- And add Has-pattern typeclass.
-- It's better to user 'SimpleGetter' instead of 'Lens',
-- because we rarely modify the interface.
--
-- @
-- class HasUserRepository env where
--   userRepositoryL :: 'SimpleGetter' env (UserRepository env)
-- @
--
-- In @signup@ function, call @findById@ method via 'invoke'.
--
-- @
-- signin :: HasUserRepository env => UserId -> Password -> RIO env (Maybe User)
-- signin userId pass = do
--   muser <- invoke (userRepositoryL . to findById) userId
--   pure $ do
--     user <- muser
--     guard (authCheck user pass)
--     pure user
-- @
--
-- In production code, inject @UserRepository@ implementation which
-- accesses database
--
-- @
-- userRepositoryImpl :: UserRepository env
-- userRepositoryImpl = UserRepository {
--   findById = ...,
--   create = ...
-- }
--
-- data ProductionEnv = ProductionEnv
-- instance HasUserRepository ProductionEnv where
--   userRepositoryL = to $ const userRepositoryImpl
-- @
--
-- In test code, inject @UserRepository@ mock implementation.
--
-- @
-- userRepositoryMock :: UserRepository env
-- userRepositoryMock = UserRepository {
--   findById = \userId -> pure $ Just (User userId "password123")
--   createUser = \user -> pure $ Just "example"
-- }
--
-- data TestEnv = TestEnv
-- instance HasUserRepository TestEnv where
--   userRepositoryL = to $ const userRepositoryMock
--
-- test :: Spec
-- test = describe "signin" $ do
--   it "return user for correct password" $ do
--     runRIO TestEnv (signin "example" "password123")
--       ``shouldReturn`` Just (User "example" "password123")
--   it "return Nothing for incorrect password" $ do
--     runRIO TestEnv (signin "example" "wrong")
--       ``shouldReturn`` Nothing
-- @

-- $decorate
-- By using 'decorate', 'decorate_', or 'decorateBefore_' function,
-- we can insert hooks before/after calling methods
--
-- Example to insert logging feature
--
-- >>> let f x y = pure (replicate x y) :: IO [String]
-- >>> let before args = putStrLn $ "args: " ++ show (toTuple args)
-- >>> let after res = putStrLn $ "ret: " ++ show res
-- >>> let decorateF = decorate_ before after f
-- >>> decorateF 2 "foo"
-- args: (2,"foo")
-- ret: Right ["foo","foo"]
-- ["foo","foo"]
--
-- Another example to decorate method with transaction management
--
-- @
-- transactional :: (Method method, MonadUnliftIO (Base method)) => (Connection -> method) -> method
-- transactional = decorate before after
--   where
--     before = do
--       conn <- liftIO $ getConnection cInfo
--       begin conn
--       pure conn
--     after conn (Left _) = liftIO $ rollback conn
--     after conn (Right _) = liftIO $ commit conn
-- @

-- | "Method" a is a function of the form
--  @a1 -> a2 -> ... -> an -> m b@
--  where @m@ is "Monad"
--
--  Typical monads in transformers package are supported.
--  If you want to support other monads (for example @M@),
--  add the following boilerplate.
--
-- @
-- instance Method (M a) where
--   Base (M a) = M
--   Ret  (M a) = a
-- @
--
--   __Caution__ Function monad @(-> r)@ cannot be an instance of 'Method'
class Monad (Base method) => Method method where
  -- | Underling monad
  --
  --   @Base (a1 -> ... -> an -> m b) = m@
  type Base method :: Type -> Type

  -- | Arguments tuple of the method
  --
  --   @Args (a1 -> ... -> an -> m b) = a1 :* ... :* an@
  type Args method :: Type

  type Args method = Nil

  -- | Return type of the method
  --
  --   @Ret  (a1 -> ... -> an -> m b) = b@
  type Ret method :: Type

  -- | Convert method to unary function
  uncurryMethod :: method -> Args method -> Base method (Ret method)
  {-# INLINE uncurryMethod #-}
  default uncurryMethod ::
    (method ~ Base method a, Args method ~ Nil, Ret method ~ a) =>
    method ->
    Args method ->
    Base method (Ret method)
  uncurryMethod method Nil = method

  -- | Reconstruct method from unary function
  curryMethod :: (Args method -> Base method (Ret method)) -> method
  {-# INLINE curryMethod #-}
  default curryMethod ::
    (method ~ Base method a, Args method ~ Nil, Ret method ~ a) =>
    (Args method -> Base method (Ret method)) ->
    method
  curryMethod method' = method' Nil

-- | Generalization of 'join' function
{-# INLINE liftJoin #-}
liftJoin :: Method method => Base method method -> method
liftJoin mMethod = curryMethod $ \args -> do
  method <- mMethod
  uncurryMethod method args

instance Method (IO a) where
  type Base (IO a) = IO
  type Ret (IO a) = a

instance Method (RIO env a) where
  type Base (RIO env a) = RIO env
  type Ret (RIO env a) = a

instance Method (Identity a) where
  type Base (Identity a) = Identity
  type Ret (Identity a) = a

instance Method (Maybe a) where
  type Base (Maybe a) = Maybe
  type Ret (Maybe a) = a

instance Method [a] where
  type Base [a] = []
  type Ret [a] = a

instance Method (Either e a) where
  type Base (Either e a) = Either e
  type Ret (Either e a) = a

instance Method (ST s a) where
  type Base (ST s a) = ST s
  type Ret (ST s a) = a

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
  {-# INLINE uncurryMethod #-}
  uncurryMethod method (a :* args) = uncurryMethod (method a) args
  {-# INLINE curryMethod #-}
  curryMethod method' a = curryMethod (\args -> method' (a :* args))

-- | Insert hooks before/after calling the argument method
{-# INLINE decorate #-}
decorate ::
  (Method method, MonadUnliftIO (Base method)) =>
  (Args method -> Base method a) ->
  (a -> Either SomeException (Ret method) -> Base method ()) ->
  (a -> method) ->
  method
decorate before after method = curryMethod $ \args -> do
  a <- before args
  res <- tryAny (uncurryMethod (method a) args)
  case res of
    Left err -> after a res >> throwIO err
    Right v -> after a res >> pure v

-- | Insert hooks before/after calling the argument method
{-# INLINE decorate_ #-}
decorate_ ::
  (Method method, MonadUnliftIO (Base method)) =>
  (Args method -> Base method ()) ->
  (Either SomeException (Ret method) -> Base method ()) ->
  method ->
  method
decorate_ before after method = curryMethod $ \args -> do
  before args
  res <- tryAny (uncurryMethod method args)
  case res of
    Left err -> after res >> throwIO err
    Right v -> after res >> pure v

-- | Insert hooks only before calling the argument method.
--   Because it's free from 'MonadUnliftIO' constraint,
--   any methods are supported.
{-# INLINE decorateBefore_ #-}
decorateBefore_ ::
  (Method method) =>
  (Args method -> Base method ()) ->
  method ->
  method
decorateBefore_ before method = curryMethod $ \args -> do
  before args
  uncurryMethod method args

-- | invoke method taken from reader environment
{-# INLINE invoke #-}
invoke :: (MonadReader env (Base method), Method method) => SimpleGetter env method -> method
invoke getter = liftJoin (view getter)