{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module : Test.Method.Monitor
-- Description:
-- License: BSD-3
-- Maintainer: autotaker@gmail.com
-- Stability: experimental
--
-- Validating method calls by monitoring
module Test.Method.Monitor
  ( Event,
    Monitor,
    newMonitor,
    watch,
    watchBy,
    listenEventLog,
    withMonitor,
    withMonitor_,
    times,
    call,
    EventMatcher,
    LogMatcher,
  )
where

import Control.Method (Method (Args, Base, Ret), decorate)
import Data.Coerce (coerce)
import RIO
  ( MonadIO (liftIO),
    MonadUnliftIO,
    readSomeRef,
  )
import Test.Method.Matcher (Matcher)
import Test.Method.Monitor.Internal
  ( EqUptoShow (EqUptoShow),
    Event (Enter, Leave),
    Monitor (monitorTrace),
    logEvent,
    newMonitor,
    tick,
  )

-- | @watchBy fArgs fRet monitor method@ decorates @method@
-- so that @monitor@ logs the method calls.
-- This function is suited for monitoring multiple methods.
--
-- @fArgs@ and @fRet@ is converter for arguments/return values of given method.
--
-- @
-- foo :: Int -> IO String
-- foo = ...
-- bar :: Int -> String -> IO ()
-- bar = ...
--
-- data MonitorArgs = FooArgs Int | BarArgs (Int,String) deriving(Eq,Show)
-- data MonitorRet = FooRet String | BarRet () deriving(Eq, Show)
--
-- foo' :: Monitor MonitorArgs MonitorRet -> Int -> IO String
-- foo' monitor = watch monitor (FooArgs . toTuple) FooRet foo
-- bar' :: Monitor MonitorArgs MonitorRet -> Int -> String -> IO ()
-- bar' monitor = watch monitor (BarArgs . toTuple) BarRet bar
-- @
watchBy ::
  (Method method, MonadUnliftIO (Base method)) =>
  (Args method -> args) ->
  (Ret method -> ret) ->
  Monitor args ret ->
  method ->
  method
watchBy fargs fret m method = method'
  where
    method' = decorate before after (const method)
    before args = do
      t <- tick m
      logEvent m (Enter t (fargs args))
      pure t
    after t result = do
      t' <- tick m
      logEvent m (Leave t' t $ coerce $ fmap fret result)

-- | Simplified version of 'watchBy'. It is suitable to monitor single method.
watch ::
  (Method method, MonadUnliftIO (Base method)) =>
  Monitor (Args method) (Ret method) ->
  method ->
  method
watch = watchBy id id

-- | Get current event logs from monitor
listenEventLog :: MonadIO m => Monitor args ret -> m [Event args ret]
listenEventLog m = reverse <$> readSomeRef (monitorTrace m)

-- | @'times' countMatcher eventMatcher@ counts events that matches @eventMatcher@,
--   and then the count matches @countMatcher@
times :: Matcher Int -> EventMatcher args ret -> LogMatcher args ret
times countMatcher eventMatcher =
  countMatcher . length . filter eventMatcher

-- | @'call' matcher@ matches method call whose arguments matches @matcher@
call :: Matcher args -> EventMatcher args ret
call argsM (Enter _ args) = argsM args
call _ Leave {} = False

type LogMatcher args ret = Matcher [Event args ret]

type EventMatcher args ret = Matcher (Event args ret)

-- | @withMonitor f@ calls @f@ with 'Monitor',
-- and then returns monitored event logs during the function call
-- in addition to the return value of the function call
withMonitor :: MonadIO m => (Monitor args ret -> m a) -> m (a, [Event args ret])
withMonitor f = do
  monitor <- liftIO newMonitor
  r <- f monitor
  logs <- listenEventLog monitor
  pure (r, logs)

-- | @withMonitor_ f@ calls @f@ with 'Monitor', and returns event logs during the call.
withMonitor_ :: MonadIO m => (Monitor args ret -> m ()) -> m [Event args ret]
withMonitor_ f = do
  monitor <- liftIO newMonitor
  f monitor
  listenEventLog monitor