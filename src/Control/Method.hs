{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Method where

import Control.Exception (SomeException)
import Data.Kind (Type)
import RIO (MonadUnliftIO, RIO, throwIO, tryAny)

class Method method where
  type Base method :: Type -> Type
  type Args method :: Type
  type Ret method :: Type
  uncurryMethod :: method -> Args method -> Base method (Ret method)
  curryMethod :: (Args method -> Base method (Ret method)) -> method

instance Method (IO a) where
  type Base (IO a) = IO
  type Args (IO a) = Nil
  type Ret (IO a) = a
  uncurryMethod method Nil = method
  curryMethod method' = method' Nil

instance Method (RIO env a) where
  type Base (RIO env a) = RIO env
  type Args (RIO env a) = Nil
  type Ret (RIO env a) = a
  uncurryMethod method Nil = method
  curryMethod method' = method' Nil

instance Method b => Method (a -> b) where
  type Base (a -> b) = Base b
  type Args (a -> b) = a :* Args b
  type Ret (a -> b) = Ret b
  uncurryMethod method (a :* args) = uncurryMethod (method a) args
  curryMethod method' a = curryMethod (\args -> method' (a :* args))

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