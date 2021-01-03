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
  | Leave {eventTick :: !Tick, eventEnterTick :: !Tick, eventRet :: !ret}
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

data Monitor method = Monitor
  { monitorTrace :: !(SomeRef [MethodEvent method]),
    monitorClock :: !Clock
  }

newMonitor :: Clock -> IO (Monitor method)
newMonitor clock = Monitor <$> newSomeRef [] <*> pure clock

newClock :: IO Clock
newClock = newIORef (Tick 0)

tick :: MonadIO m => Monitor method -> m Tick
tick Monitor {monitorClock = clock} = do
  t <- readIORef clock
  writeIORef clock $! succ t
  pure t

logEvent :: MonadIO m => Monitor method -> MethodEvent method -> m ()
logEvent Monitor {monitorTrace = tr} event = modifySomeRef tr (event :)

monitor ::
  (Method method, MonadUnliftIO (Base method)) =>
  Clock ->
  method ->
  IO (Monitor method, method)
monitor clock method = do
  m <- newMonitor clock
  let method' = decorate before after method
      before args = do
        t <- tick m
        logEvent m (Enter t args)
        pure t
      after t result = do
        t' <- tick m
        logEvent m (Leave t' t (coerce result))
  pure (m, method')

getEventLog :: MonadIO m => Monitor method -> m [MethodEvent method]
getEventLog m = reverse <$> readSomeRef (monitorTrace m)

interleave :: [Event arg1 ret1] -> [Event arg2 ret2] -> [Event (Either arg1 arg2) (Either ret1 ret2)]
interleave events [] = map liftLeft events
interleave [] events = map liftRight events
interleave (e1 : events1) (e2 : events2)
  | eventTick e1 <= eventTick e2 = liftLeft e1 : interleave events1 (e2 : events2)
  | otherwise = liftRight e2 : interleave (e1 : events1) events2

liftLeft :: Event a1 a2 -> Event (Either a1 b1) (Either a2 b2)
liftLeft (Enter t args) = Enter t (Left args)
liftLeft (Leave t t' ret) = Leave t t' (Left ret)

liftRight :: Event b1 b2 -> Event (Either a1 b1) (Either a2 b2)
liftRight (Enter t args) = Enter t (Right args)
liftRight (Leave t t' ret) = Leave t t' (Right ret)

type LogMatcher args ret = Matcher [Event args ret]

type EventMatcher args ret = Matcher (Event args ret)

times :: Matcher Int -> EventMatcher args ret -> LogMatcher args ret
times countMatcher eventMatcher =
  countMatcher . length . filter eventMatcher

call :: Matcher args -> EventMatcher args ret
call argsM (Enter _ args) = argsM args
call _ Leave {} = False