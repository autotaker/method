{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Method.Monitor.Internal where

import Data.Typeable (typeOf)
import RIO
  ( IORef,
    MonadIO,
    SomeException,
    SomeRef,
    Typeable,
    newIORef,
    newSomeRef,
    readIORef,
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

type LogMatcher args ret = Matcher [Event args ret]

type EventMatcher args ret = Matcher (Event args ret)

times :: Matcher Int -> EventMatcher args ret -> LogMatcher args ret
times countMatcher eventMatcher =
  countMatcher . length . filter eventMatcher

call :: Matcher args -> EventMatcher args ret
call argsM (Enter _ args) = argsM args
call _ Leave {} = False