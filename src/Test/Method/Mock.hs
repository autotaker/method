{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module : Test.Method.Mock
-- Description:
-- License: BSD-3
-- Maintainer: autotaker@gmail.com
-- Stability: experimental
--
-- DSL to generate mock methods.
module Test.Method.Mock
  ( Mock,
    MockSpec,
    mockup,
    thenReturn,
    thenAction,
    thenMethod,
    throwNoStubShow,
    throwNoStub,
    NoStubException,
  )
where

import Control.Method
  ( Method (Args, Base, Ret, curryMethod, uncurryMethod),
    TupleLike (AsTuple, toTuple),
  )
import Data.Data (Typeable)
import RIO (Exception, MonadThrow (throwM))
import RIO.List (find)
import RIO.Writer (MonadWriter (tell), Writer, execWriter)
import Test.Method.Matcher (Matcher)

type Mock method = Writer (MockSpec method) ()

data MockSpec method
  = Empty
  | Combine (MockSpec method) (MockSpec method)
  | MockSpec (Matcher (Args method)) method

newtype NoStubException = NoStubException String
  deriving (Show, Typeable)

instance Exception NoStubException

instance Semigroup (MockSpec method) where
  (<>) = Combine

instance Monoid (MockSpec method) where
  mempty = Empty

mockup :: (Method method, MonadThrow (Base method)) => Mock method -> method
mockup spec = buildMock (execWriter spec)

buildMock :: (Method method, MonadThrow (Base method)) => MockSpec method -> method
buildMock spec = fromRules $ toRules spec

thenReturn :: (Method method, Applicative (Base method)) => Matcher (Args method) -> Ret method -> Mock method
thenReturn matcher retVal =
  tell $ MockSpec matcher $ curryMethod (const $ pure retVal)

thenAction ::
  Method method =>
  Matcher (Args method) ->
  Base method (Ret method) ->
  Mock method
thenAction matcher ret =
  tell $ MockSpec matcher $ curryMethod $ const ret

throwNoStubShow ::
  ( Method method,
    Show (AsTuple (Args method)),
    MonadThrow (Base method),
    TupleLike (Args method)
  ) =>
  Matcher (Args method) ->
  Mock method
throwNoStubShow matcher =
  tell $
    MockSpec matcher $
      curryMethod $
        throwM . NoStubException . show . toTuple

throwNoStub :: (Method method, MonadThrow (Base method)) => (Args method -> String) -> (Args method -> Bool) -> Mock method
throwNoStub fshow matcher =
  tell $
    MockSpec matcher $
      curryMethod $
        throwM . NoStubException . fshow

thenMethod :: (Method method) => Matcher (Args method) -> method -> Mock method
thenMethod matcher method = tell $ MockSpec matcher method

fromRules :: (Method method, MonadThrow (Base method)) => [(Matcher (Args method), method)] -> method
fromRules rules = curryMethod $ \args ->
  let ret = find (\(matcher, _) -> matcher args) rules
   in case ret of
        Just (_, method) -> uncurryMethod method args
        Nothing -> throwM $ NoStubException "no mock"

toRules :: MockSpec method -> [(Matcher (Args method), method)]
toRules = reverse . go []
  where
    go acc Empty = acc
    go acc (Combine a b) = go (go acc a) b
    go acc (MockSpec matcher ret) = (matcher, ret) : acc
