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
  )
where

import Control.Method
  ( Method (Args, Base, Ret, curryMethod, uncurryMethod),
    TupleLike (AsTuple, toTuple),
  )
import RIO.List (find)
import RIO.Writer (MonadWriter (tell), Writer, execWriter)
import Test.Method.Matcher (Matcher)

type Mock method = Writer (MockSpec method) ()

data MockSpec method
  = Empty
  | Combine (MockSpec method) (MockSpec method)
  | MockSpec (Matcher (Args method)) method

instance Semigroup (MockSpec method) where
  (<>) = Combine

instance Monoid (MockSpec method) where
  mempty = Empty

-- | generate a method from Mock DSL.
-- Mock DSL consists of rules.
-- On a call of generated method, the first rule matched the arguments is applied.
mockup :: (Method method) => Mock method -> method
mockup spec = buildMock (execWriter spec)

buildMock :: Method method => MockSpec method -> method
buildMock spec = fromRules $ toRules spec

-- | @matcher `'thenReturn'` value@ means the method return @value@
-- if the arguments matches @matcher@.
thenReturn :: (Method method, Applicative (Base method)) => Matcher (Args method) -> Ret method -> Mock method
thenReturn matcher retVal =
  tell $ MockSpec matcher $ curryMethod (const $ pure retVal)

-- | @matcher `'thenAction'` action@ means the method executes @action@
-- if the arguments matches @matcher@.
thenAction ::
  Method method =>
  Matcher (Args method) ->
  Base method (Ret method) ->
  Mock method
thenAction matcher ret =
  tell $ MockSpec matcher $ curryMethod $ const ret

-- | @matcher `'thenMethod'` action@ means the method call @method@ with the arguments
-- if the arguments matches @matcher@.
thenMethod :: (Method method) => Matcher (Args method) -> method -> Mock method
thenMethod matcher method = tell $ MockSpec matcher method

-- | @'throwNoStubShow' matcher@ means the method raises a runtime exception
-- if the arguments matches @matcher@. The argument tuple is converted to 'String' by
-- using 'show' function.
throwNoStubShow ::
  ( Method method,
    Show (AsTuple (Args method)),
    TupleLike (Args method)
  ) =>
  Matcher (Args method) ->
  Mock method
throwNoStubShow matcher =
  tell $
    MockSpec matcher $
      curryMethod $
        error . ("no stub found for argument: " <>) . show . toTuple

-- | @'throwNoStubShow' fshow matcher@ means the method raises runtime exception
-- if the arguments matches @matcher@. The argument tuple is converted to 'String' by
-- using 'fshow' function.
throwNoStub :: (Method method) => (Args method -> String) -> (Args method -> Bool) -> Mock method
throwNoStub fshow matcher =
  tell $
    MockSpec matcher $
      curryMethod $ error . ("no stub found for argument: " <>) . fshow

fromRules :: Method method => [(Matcher (Args method), method)] -> method
fromRules rules = curryMethod $ \args ->
  let ret = find (\(matcher, _) -> matcher args) rules
   in case ret of
        Just (_, method) -> uncurryMethod method args
        Nothing -> error "no stub. For debugging, use `throwNoStubShow anything`"

toRules :: MockSpec method -> [(Matcher (Args method), method)]
toRules = reverse . go []
  where
    go acc Empty = acc
    go acc (Combine a b) = go (go acc a) b
    go acc (MockSpec matcher ret) = (matcher, ret) : acc
