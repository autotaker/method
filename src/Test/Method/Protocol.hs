{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Method.Protocol
  ( protocol,
    ProtocolM,
    ProtocolEnv,
    Call,
    CallArgs,
    CallId,
    IsMethodName,
    lookupMock,
    lookupMockWithShow,
    decl,
    whenArgs,
    thenMethod,
    thenAction,
    thenReturn,
    dependsOn,
    verify,
  )
where

import Control.Method
  ( Method (Args, Base, curryMethod, uncurryMethod),
    TupleLike (AsTuple, toTuple),
  )
import Control.Monad.Trans.State.Strict (StateT, execStateT, state)
import Data.Maybe (fromJust)
import Data.Typeable
  ( Typeable,
    cast,
    typeOf,
  )
import RIO (IORef, MonadIO (liftIO), Set, forM_, newIORef, on, readIORef, unless, writeIORef, (&))
import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.Set as S
import Test.Method.Behavior (Behave (Condition, MethodOf, thenMethod), thenAction, thenReturn)
import Test.Method.Matcher (ArgsMatcher (EachMatcher, args), Matcher)
import Unsafe.Coerce (unsafeCoerce)

newtype CallId = CallId {unCallId :: Int}
  deriving (Eq, Ord, Show)

data CallArgs f m = CallArgs
  { methodName :: f m,
    argsMatcher :: Matcher (Args m)
  }

data Call f m = Call
  { argsSpec :: CallArgs f m,
    retSpec :: m,
    dependCall :: [CallId]
  }

data SomeCall f where
  SomeCall :: IsMethodName f m => Call f m -> SomeCall f

data SomeMethodName f where
  SomeMethodName :: IsMethodName f m => f m -> SomeMethodName f

instance Eq (SomeMethodName f) where
  SomeMethodName x == SomeMethodName y =
    case cast y of
      Just y' -> x == y'
      Nothing -> False

instance Ord (SomeMethodName f) where
  compare (SomeMethodName x) (SomeMethodName y) =
    compare (typeOf x) (typeOf y) <> case cast y of
      Just y' -> compare x y'
      Nothing -> LT

instance Show (SomeMethodName f) where
  show (SomeMethodName x) = show x

data MethodCallAssoc f where
  MethodCallAssoc ::
    forall f m.
    (Typeable (f m), Show (f m)) =>
    { assocCalls :: [(CallId, Call f m)],
      assocCounter :: IORef Int
    } ->
    MethodCallAssoc f

-- | @'ProtocolEnv' f@ provides mock methods, where @f@ is a GADT functor that
--   represents the set of dependent methods.
data ProtocolEnv f = ProtocolEnv
  { callSpecs :: [(CallId, SomeCall f)],
    methodEnv :: M.Map (SomeMethodName f) (MethodCallAssoc f),
    calledIdSetRef :: IORef (Set CallId)
  }

newtype ProtocolM f a
  = ProtocolM (StateT ([(CallId, SomeCall f)], CallId) IO a)

deriving instance Functor (ProtocolM f)

deriving instance Applicative (ProtocolM f)

deriving instance Monad (ProtocolM f)

getMethodName :: SomeCall f -> SomeMethodName f
getMethodName (SomeCall Call {argsSpec = CallArgs {methodName = name}}) = SomeMethodName name

-- | Build 'ProtocolEnv' from Protocol DSL.
protocol :: ProtocolM f a -> IO (ProtocolEnv f)
protocol (ProtocolM dsl) = do
  (specs, _) <- execStateT dsl ([], CallId 0)
  assocList <-
    specs
      & map (\(callId, call) -> (getMethodName call, callId, call))
      & L.sortOn (\(x, y, _) -> (x, y))
      & L.groupBy ((==) `on` (\(x, _, _) -> x))
      & mapM
        ( \l ->
            case head l of
              (SomeMethodName (name :: f m), _, _) -> do
                ref <- newIORef 0
                pure
                  ( SomeMethodName name,
                    MethodCallAssoc @f @m
                      [(callId, unsafeCoerce call) | (_, callId, SomeCall call) <- l]
                      ref
                  )
        )
  ref <- newIORef S.empty
  pure
    ProtocolEnv
      { callSpecs = specs,
        methodEnv = M.fromList assocList,
        calledIdSetRef = ref
      }

tick :: MonadIO m => IORef Int -> m Int
tick ref = liftIO $ do
  x <- readIORef ref
  writeIORef ref (x + 1)
  pure x

type IsMethodName f m = (Typeable (f m), Eq (f m), Ord (f m), Show (f m))

-- | Get the mock method by method name.
--   Return a unstubed method (which throws exception for every call)
--   if the behavior of the method is unspecified by ProtocolEnv
lookupMock ::
  forall f m.
  (IsMethodName f m, Show (AsTuple (Args m)), TupleLike (Args m), Method m, MonadIO (Base m)) =>
  -- | name of method
  f m ->
  ProtocolEnv f ->
  m
lookupMock = lookupMockWithShow (show . toTuple)

-- | Get the mock method by method name.
--   Return a unstubed method (which throws exception for every call)
--   if the behavior of the method is unspecified by ProtocolEnv.
--   Use this function only if you want to customize
--   show implementation for the argument of the method.
lookupMockWithShow ::
  forall f m.
  (IsMethodName f m, Method m, MonadIO (Base m)) =>
  -- | show function for the argument of method
  (Args m -> String) ->
  -- | name of method
  f m ->
  ProtocolEnv f ->
  m
lookupMockWithShow fshow name ProtocolEnv {..} =
  case M.lookup (SomeMethodName name) methodEnv of
    Nothing -> curryMethod $ \_ ->
      error $
        "0-th call of method " <> show name <> " is unspecified"
    Just MethodCallAssoc {assocCalls = assocCalls', ..} ->
      let assocCalls = unsafeCoerce assocCalls' :: [(CallId, Call f m)]
       in curryMethod $ \xs -> do
            i <- tick assocCounter
            unless (i < length assocCalls) $
              error $ show i <> "-th call of method " <> show name <> " is unspecified"
            let (callId, Call {..}) = assocCalls !! i
                CallArgs {..} = argsSpec
            unless (argsMatcher xs) $
              error $
                "unexpected argument of " <> show i <> "-th call of method " <> show name <> ": "
                  <> fshow xs
            calledIdSet <- liftIO $ readIORef calledIdSetRef
            forM_ dependCall $ \callId' -> do
              unless (S.member callId' calledIdSet) $
                let call = fromJust $ L.lookup callId' callSpecs
                 in error $ "dependent method " <> show (getMethodName call) <> " is not called: " <> show callId'
            liftIO $ writeIORef calledIdSetRef $! S.insert callId calledIdSet
            uncurryMethod retSpec xs

-- | Declare a method call specification. It returns the call id of the method call.
decl :: (IsMethodName f m) => Call f m -> ProtocolM f CallId
decl call = ProtocolM $
  state $ \(l, callId@(CallId i)) ->
    (callId, ((callId, SomeCall call) : l, CallId (i + 1)))

-- | Specify the argument condition of a method call
whenArgs :: ArgsMatcher (Args m) => f m -> EachMatcher (Args m) -> CallArgs f m
whenArgs name matcher = CallArgs {methodName = name, argsMatcher = args matcher}

instance Behave (Call f m) where
  type Condition (Call f m) = CallArgs f m
  type MethodOf (Call f m) = m
  thenMethod lhs m =
    Call
      { argsSpec = lhs,
        retSpec = m,
        dependCall = []
      }

-- | Specify on which method calls the call depends.
dependsOn :: Call f m -> [CallId] -> Call f m
dependsOn call depends = call {dependCall = depends <> dependCall call}

-- | Verify that all method calls specified by Protocol DSL are fired.
verify :: ProtocolEnv f -> IO ()
verify ProtocolEnv {..} = do
  forM_ (M.assocs methodEnv) $ \(name, MethodCallAssoc {..}) -> do
    n <- readIORef assocCounter
    let expected = length assocCalls
    unless (n == expected) $
      error $
        "method " <> show name <> " should be called " <> show expected
          <> " times, but actually is called "
          <> show n
          <> " times"