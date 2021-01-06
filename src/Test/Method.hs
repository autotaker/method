{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Test.Method
  ( -- $usage

    -- * Mock

    -- ** Usage
    -- $mock

    -- ** References
    mockup,
    thenReturn,
    thenAction,
    thenMethod,
    throwNoStubShow,
    throwNoStub,
    NoStubException (NoStubException),

    -- * Monitor

    -- ** Usage
    -- $monitor
    module Test.Method.Monitor,

    -- * Matcher
    module Test.Method.Matcher,
  )
where

import Test.Method.Matcher
  ( ArgsMatcher (..),
    Matcher,
    TupleLike (..),
    anything,
    when,
  )
import Test.Method.Mock
  ( Mock,
    MockSpec,
    NoStubException (NoStubException),
    mockup,
    thenAction,
    thenMethod,
    thenReturn,
    throwNoStub,
    throwNoStubShow,
  )
import Test.Method.Monitor
  ( Event,
    EventMatcher,
    LogMatcher,
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

-- $usage
-- This module provides DSLs for mocking
-- methods and for validating method calls

-- $mock
--
-- @
-- fizzbuzz :: Int -> IO String
-- fizzbuzz = 'mockup' $ do
--   'when' ('args' (\x -> mod x 15 == 0)) `'thenReturn'` "fizzbuzz"
--   'when' ('args' (\x -> mod x 3 == 0)) `'thenReturn'` "fizz"
--   'when' ('args' (\x -> mod x 5 == 0)) `'thenReturn'` "buzz"
--   'when' ('args' (>=0)) `'thenMethod'` (\x -> pure $ show x)
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
-- *** Exception: NoStubException "-1"

-- @

-- $monitor
