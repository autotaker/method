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
  ( NoStubException (NoStubException),
    mockup,
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
--
-- @
-- type ExampleMethod = Int -> String -> IO String
-- example :: ExampleMethod
-- example n s | n < 0 = throwString "negative n"
--             | otherwise = pure $ concat $ replicate n s
--
-- doit :: ExampleMethod -> IO ()
-- doit example = (do
--   example 2 "foo" >>= putStrLn
--   example 3 "foo" >>= putStrLn
--   example (-1) "bar" >>= putStrLn
--   example 3 "bar" >>= putStrLn) `catchAny` (const $ pure ())
-- @
--
-- @
-- spec :: Spec
-- spec = describe "doit" $ do
--   before ('withMonitor_' $ \\monitor -> doit ('watch' monitor example))
--
--   it "calls example _ \"foo\" twice" $ \\logs -> do
--     logs `'shouldSatisfy'` ((==2) `'times'` 'call' ('args' ('anything', (=="foo"))))
--
--   it "calls example (-1) \"bar\" once" $ \\logs -> do
--     logs `'shouldSatisfy'` ((==1) `'times'` 'call' ('args' ((==(-1)), (=="bar"))))
--
--   it "does not call example 3 \"bar\" " $ \\logs -> do
--     logs `'shouldSatisfy'` ((==0) `'times'` 'call' ('args' ((==3), (=="bar"))))
-- @
