{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Test.Method.Monitor where

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

newtype Tick = Tick {unTick :: Int}
  deriving (Eq, Ord, Show, Enum)

data Event args ret
  = Enter {eventTick :: !Tick, eventArgs :: !args}
  | Leave
      { eventTick :: !Tick,
        eventEnterTick :: !Tick,
        eventRet :: !(Either (EqUptoShow SomeException) ret)
      }
  deriving (Eq, Ord, Show)

type MethodEvent method = Event (Args method) (Either (EqUptoShow SomeException) (Ret method))

type Clock = IORef Tick

newtype ShowType a = ShowType a
  deriving (Eq, Ord)

newtype EqUptoShow a = EqUptoShow a
  deriving (Show)

instance Show a => Eq (EqUptoShow a) where
  a == b = show a == show b

instance Show a => Ord (EqUptoShow a) where
  compare a b = compare (show a) (show b)

instance Typeable a => Show (ShowType a) where
  show (ShowType a) = show (typeOf a)

data Monitor args ret = Monitor
  { monitorTrace :: !(SomeRef [Event args ret]),
    monitorClock :: !Clock
  }

newMonitor :: IO (Monitor args ret)
newMonitor = Monitor <$> newSomeRef [] <*> newIORef (Tick 0)

tick :: MonadIO m => Monitor args ret -> m Tick
tick Monitor {monitorClock = clock} = do
  t <- readIORef clock
  writeIORef clock $! succ t
  pure t

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

type LogMatcher args ret = Matcher [Event args ret]

type EventMatcher args ret = Matcher (Event args ret)

times :: Matcher Int -> EventMatcher args ret -> LogMatcher args ret
times countMatcher eventMatcher =
  countMatcher . length . filter eventMatcher

call :: Matcher args -> EventMatcher args ret
call argsM (Enter _ args) = argsM args
call _ Leave {} = False