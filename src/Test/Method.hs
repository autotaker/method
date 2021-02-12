{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Module : Test.Method
-- License: BSD-3
-- Maintainer: autotaker@gmail.com
-- Stability: experimental
module Test.Method
  ( -- $usage

    -- * Mock

    -- ** Usage
    -- $mock

    -- ** References
    mockup,
    tell,
    thenReturn,
    thenAction,
    thenMethod,
    throwNoStubShow,
    throwNoStub,

    -- * Monitor

    -- ** Usage
    -- $monitor

    -- ** References
    Monitor,
    Event,
    watchBy,
    watch,
    withMonitor,
    withMonitor_,

    -- *** Matcher for events
    call,
    times,

    -- *** Procedual api for monitor
    newMonitor,
    listenEventLog,

    -- * Protocol

    -- ** References
    protocol,
    ProtocolM,
    ProtocolEnv,
    CallId,
    decl,
    whenArgs,
    dependsOn,
    lookupMock,
    lookupMockS,
    verify,

    -- * Matcher

    -- ** References

    -- *** Basics
    Matcher,
    anything,
    when,

    -- *** Matcher for method arguments
    TupleLike (AsTuple, fromTuple, toTuple),
    ArgsMatcher (args),
    args',
  )
where

import Test.Method.Matcher
  ( ArgsMatcher (..),
    Matcher,
    TupleLike (..),
    anything,
    args',
    when,
  )
import Test.Method.Mock
  ( mockup,
    tell,
    thenAction,
    thenMethod,
    thenReturn,
    throwNoStub,
    throwNoStubShow,
  )
import Test.Method.Monitor
  ( Event,
    Monitor,
    call,
    listenEventLog,
    newMonitor,
    times,
    watch,
    watchBy,
    withMonitor,
    withMonitor_,
  )
import Test.Method.Protocol
  ( CallId,
    ProtocolEnv,
    ProtocolM,
    decl,
    dependsOn,
    lookupMock,
    lookupMockS,
    protocol,
    verify,
    whenArgs,
  )

-- $usage
-- This module provides DSLs for mocking
-- methods and for validating method calls

-- $mock
--
-- @
-- fizzbuzz :: Int -> IO String
-- fizzbuzz = 'mockup' $ do
--   'tell' $ 'when' ('args' (\\x -> mod x 15 == 0)) `'thenReturn'` "fizzbuzz"
--   'tell' $ 'when' ('args' (\\x -> mod x 3 == 0)) `'thenReturn'` "fizz"
--   'tell' $ 'when' ('args' (\\x -> mod x 5 == 0)) `'thenReturn'` "buzz"
--   'tell' $ 'when' ('args' (>=0)) `'thenMethod'` (\\x -> pure $ show x)
--   'throwNoStubShow' $ 'when' 'anything'
-- @
--
-- >>> fizzbuzz 0
-- "fizzbuzz"
-- >>> fizzbuzz 1
-- "1"
-- >>> fizzbuzz 3
-- "fizz"
-- >>> fizzbuzz 5
-- "buzz"
-- >>> fizzbuzz (-1)
-- *** Exception: no stub found for argument: -1
-- CallStack (from HasCallStack):
--  error, called at src/Test/Method/Mock.hs:98:9 in method-0.2.0.0-inplace:Test.Method.Mock"

-- @

-- $monitor
--
-- Production code
--
-- @
-- type ExampleMethod env = Int -> String -> RIO env ()
--
-- class HasExampleMethod env where
--   exampleL :: Lens\' env (ExampleMethod env)
--
-- doit :: HasExampleMethod env => RIO env ()
-- doit = (do
--   invoke exampleL 2 "foo"
--   invoke exampleL 3 "foo"
--   invoke exampleL (-1) "bar"
--   invoke exampleL 3 "bar") `catchAny` (const $ pure ())
-- @
--
-- Test code
--
-- @
-- data Env = Env { _example :: ExampleMethod env }
-- makeLenses Env''
--
-- instance HasExampleMethod Env where
--   exampleL = example
--
-- exampleMock :: ExampleMethod
-- exampleMock = 'mockup' $ do
--   'tell' $ 'when' ('args' ((<0), 'anything')) `'thenAction'` throwString "negative n"
--   'tell' $ 'when' 'anything' `'thenReturn'` ()
--
-- env = Env exampleMock
--
-- spec :: Spec
-- spec = describe "doit" $ do
--   before $ 'withMonitor_' $ \\monitor -> runRIO env $ local (exampleL %~ 'watch' monitor) doit
--
--   it "calls example _ \\\"foo\\\" twice" $ \\logs -> do
--     logs `'shouldSatisfy'` ((==2) `'times'` 'call' ('args' ('anything', (=="foo"))))
--
--   it "calls example (-1) \\\"bar\\\" once" $ \\logs -> do
--     logs `'shouldSatisfy'` ((==1) `'times'` 'call' ('args' ((==(-1)), (=="bar"))))
--
--   it "does not call example 3 \\\"bar\\\" " $ \\logs -> do
--     logs `'shouldSatisfy'` ((==0) `'times'` 'call' ('args' ((==3), (=="bar"))))
-- @
