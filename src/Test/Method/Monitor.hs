{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Test.Method.Monitor
  ( Event (..),
    Tick (..),
    Monitor,
    newMonitor,
    logEvent,
    watch,
    watch',
    getEventLog,
    times,
    call,
    EventMatcher,
    LogMatcher,
  )
where

import Control.Method (Method (Args, Base, Ret), decorate)
import Data.Coerce (coerce)
import Data.Typeable (typeOf)
import RIO
  ( IORef,
    MonadIO,
    MonadUnliftIO,
    SomeException,
    SomeRef,
    Typeable,
    modifySomeRef,
    newIORef,
    newSomeRef,
    readIORef,
    readSomeRef,
    writeIORef,
  )
import Test.Method.Matcher (Matcher)
import Test.Method.Monitor.Internal

logEvent :: MonadIO m => Monitor args ret -> Event args ret -> m ()
logEvent Monitor {monitorTrace = tr} event = modifySomeRef tr (event :)

watch ::
  (Method method, MonadUnliftIO (Base method)) =>
  (Args method -> args) ->
  (Ret method -> ret) ->
  Monitor args ret ->
  method ->
  IO method
watch fargs fret m method = do
  let method' = decorate before after (const method)
      before args = do
        t <- tick m
        logEvent m (Enter t (fargs args))
        pure t
      after t result = do
        t' <- tick m
        logEvent m (Leave t' t $ coerce $ fmap fret result)
  pure method'

watch' ::
  (Method method, MonadUnliftIO (Base method)) =>
  Monitor (Args method) (Ret method) ->
  method ->
  IO method
watch' = watch id id

getEventLog :: MonadIO m => Monitor args ret -> m [Event args ret]
getEventLog m = reverse <$> readSomeRef (monitorTrace m)
