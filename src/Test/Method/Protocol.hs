{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Method.Protocol
  ( protocol,
    ProtocolM,
    lookupMock,
    decl,
    whenArgs,
    thenReturn,
    dependsOn,
    verify,
  )
where

import Control.Method
  ( Method (Args, Base, Ret, curryMethod, uncurryMethod),
    TupleLike (AsTuple, toTuple),
  )
import Control.Monad.Trans.State.Strict (StateT, execStateT, state)
import Data.Maybe (fromJust)
import Data.Typeable
  ( Typeable,
    cast,
    eqT,
    typeOf,
    type (:~:) (Refl),
  )
import RIO (IORef, MonadIO (liftIO), Set, forM_, newIORef, on, readIORef, unless, writeIORef, (&))
import qualified RIO.List as L
import qualified RIO.Set as S
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
  SomeCall :: (Eq (f m), Ord (f m), Show (f m), Typeable (f m)) => Call f m -> SomeCall f

data SomeMethodName f where
  SomeMethodName :: (Eq (f m), Ord (f m), Show (f m), Typeable (f m)) => f m -> SomeMethodName f

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
    (Typeable (f m), Show (f m)) =>
    { assocMethodName :: f m,
      assocCalls :: [(CallId, Call f m)],
      assocCounter :: IORef Int
    } ->
    MethodCallAssoc f

data ProtocolEnv f = ProtocolEnv
  { callSpecs :: [(CallId, SomeCall f)],
    methodAssocList :: [MethodCallAssoc f],
    calledIdSetRef :: IORef (Set CallId)
  }

type ProtocolM f a = StateT ([(CallId, SomeCall f)], CallId) IO a

getMethodName :: SomeCall f -> SomeMethodName f
getMethodName (SomeCall Call {argsSpec = CallArgs {methodName = name}}) = SomeMethodName name

protocol :: ProtocolM f () -> IO (ProtocolEnv f)
protocol dsl = do
  (specs, _) <- execStateT dsl ([], CallId 0)
  assocList <-
    specs
      & map (\(callId, call) -> (getMethodName call, callId, call))
      & L.sortOn (\(x, y, _) -> (x, y))
      & L.groupBy ((==) `on` (\(x, _, _) -> x))
      & mapM
        ( \l ->
            case head l of
              (SomeMethodName name, _, _) -> do
                ref <- newIORef 0
                pure
                  MethodCallAssoc
                    { assocMethodName = name,
                      assocCalls = [(callId, unsafeCoerce call) | (_, callId, SomeCall call) <- l],
                      assocCounter = ref
                    }
        )
  ref <- newIORef S.empty
  pure
    ProtocolEnv
      { callSpecs = specs,
        methodAssocList = assocList,
        calledIdSetRef = ref
      }

tick :: MonadIO m => IORef Int -> m Int
tick ref = liftIO $ do
  x <- readIORef ref
  writeIORef ref (x + 1)
  pure x

lookupMock :: forall f m. (Typeable (f m), Eq (f m), Show (f m), Show (AsTuple (Args m)), TupleLike (Args m), Method m, MonadIO (Base m)) => f m -> ProtocolEnv f -> m
lookupMock name ProtocolEnv {..} = go methodAssocList
  where
    go [] = curryMethod $ \_ ->
      error $
        "0-th call of method " <> show name <> " is unspecified"
    go (MethodCallAssoc {assocMethodName = name' :: f m', ..} : as) =
      case eqT @(f m) @(f m') of
        Just Refl | name' == name ->
          curryMethod $ \xs -> do
            i <- tick assocCounter
            unless (i < length assocCalls) $
              error $ show i <> "-th call of method " <> show name <> " is unspecified"
            let (callId, Call {..}) = assocCalls !! i
                CallArgs {..} = argsSpec
            unless (argsMatcher xs) $
              error $
                "unexpected argument of " <> show i <> "-th call of method " <> show name <> ": "
                  <> show (toTuple xs)
            calledIdSet <- liftIO $ readIORef calledIdSetRef
            forM_ dependCall $ \callId' -> do
              unless (S.member callId' calledIdSet) $
                let call = fromJust $ L.lookup callId' callSpecs
                 in error $ "dependent method " <> show (getMethodName call) <> " is not called: " <> show callId'
            liftIO $ writeIORef calledIdSetRef $! S.insert callId calledIdSet
            uncurryMethod retSpec xs
        _ -> go as

decl :: (Eq (f m), Ord (f m), Show (f m), Typeable (f m)) => Call f m -> ProtocolM f CallId
decl call = state $ \(l, callId@(CallId i)) ->
  (callId, ((callId, SomeCall call) : l, CallId (i + 1)))

whenArgs :: ArgsMatcher (Args m) => f m -> EachMatcher (Args m) -> CallArgs f m
whenArgs name matcher = CallArgs {methodName = name, argsMatcher = args matcher}

thenReturn :: Method m => CallArgs f m -> Ret m -> Call f m
thenReturn callArgs rVal =
  Call
    { argsSpec = callArgs,
      retSpec = curryMethod $ const (pure rVal),
      dependCall = []
    }

dependsOn :: Call f m -> [CallId] -> Call f m
dependsOn call depends = call {dependCall = depends <> dependCall call}

verify :: ProtocolEnv f -> IO ()
verify ProtocolEnv {..} = do
  forM_ methodAssocList $ \MethodCallAssoc {..} -> do
    n <- readIORef assocCounter
    let expected = length assocCalls
    unless (n == expected) $
      error $
        "method " <> show assocMethodName <> " should be called " <> show expected
          <> " times, but actually is called "
          <> show n
          <> " times"