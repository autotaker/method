{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Method.Monitor.Internal where

import Data.Typeable (typeOf)
import RIO
  ( IORef,
    MonadIO,
    SomeException,
    SomeRef,
    Typeable,
    modifySomeRef,
    newIORef,
    newSomeRef,
    readIORef,
    writeIORef,
  )

-- | 'Tick' represents call identifier
newtype Tick = Tick {unTick :: Int}
  deriving (Eq, Ord, Show, Enum)

-- | @'Event' args ret@ is a function call event
data Event args ret
  = Enter {eventTick :: !Tick, eventArgs :: !args}
  | Leave
      { eventTick :: !Tick,
        eventEnterTick :: !Tick,
        eventRet :: !(Either (EqUptoShow SomeException) ret)
      }
  deriving (Eq, Ord, Show)

type Clock = IORef Tick

-- | newtype to implement show instance which shows its type.
newtype ShowType a = ShowType a
  deriving (Eq, Ord)

instance Typeable a => Show (ShowType a) where
  show (ShowType a) = show (typeOf a)

-- | newtype to compare values via 'show'
newtype EqUptoShow a = EqUptoShow a

instance Show a => Show (EqUptoShow a) where
  show (EqUptoShow a) = show a

instance Show a => Eq (EqUptoShow a) where
  a == b = show a == show b

instance Show a => Ord (EqUptoShow a) where
  compare a b = compare (show a) (show b)

-- | @Monitor arg ret@ is an event monitor of methods,
-- which logs method calls.
data Monitor args ret = Monitor
  { monitorTrace :: !(SomeRef [Event args ret]),
    monitorClock :: !Clock
  }

-- | Generate new instance of 'Monitor'
newMonitor :: IO (Monitor args ret)
newMonitor = Monitor <$> newSomeRef [] <*> newIORef (Tick 0)

-- | Increment the clock and return the current tick.
tick :: MonadIO m => Monitor args ret -> m Tick
tick Monitor {monitorClock = clock} = do
  t <- readIORef clock
  writeIORef clock $! succ t
  pure t

-- | logs an event
logEvent :: MonadIO m => Monitor args ret -> Event args ret -> m ()
logEvent Monitor {monitorTrace = tr} event = modifySomeRef tr (event :)