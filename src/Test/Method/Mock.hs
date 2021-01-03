{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Method.Mock where

import Control.Method
  ( Method (Args, Base, Ret, curryMethod),
  )
import Data.Data (Typeable)
import RIO (Exception, MonadThrow (throwM))
import RIO.List (find)
import RIO.Writer (MonadWriter (tell), Writer, execWriter)
import Test.Method.Matcher (Matcher)

type Mock method = Writer (MockSpec method) ()

data MockSpec method
  = Empty
  | Combine (MockSpec method) (MockSpec method)
  | MockSpec (Matcher (Args method)) (Base method (Ret method))

newtype NoStubException = NoStubException String
  deriving (Show, Typeable)

instance Exception NoStubException

instance Semigroup (MockSpec method) where
  (<>) = Combine

instance Monoid (MockSpec method) where
  mempty = Empty

mockup :: (Method method, MonadThrow (Base method)) => Mock method -> method
mockup spec = buildMock (execWriter spec)

buildMock :: (Method method, MonadThrow (Base method)) => MockSpec method -> method
buildMock spec = fromRules $ toRules spec

thenReturn :: Applicative (Base method) => Matcher (Args method) -> Ret method -> Mock method
thenReturn matcher retVal = tell $ MockSpec matcher (pure retVal)

thenAction :: Matcher (Args method) -> Base method (Ret method) -> Mock method
thenAction matcher ret = tell $ MockSpec matcher ret

fromRules :: (Method method, MonadThrow (Base method)) => [(Matcher (Args method), Base method (Ret method))] -> method
fromRules rules = curryMethod $ \args ->
  let ret = find (\(matcher, _) -> matcher args) rules
   in case ret of
        Just (_, v) -> v
        Nothing -> throwM $ NoStubException "no mock"

toRules :: MockSpec method -> [(Matcher (Args method), Base method (Ret method))]
toRules = reverse . go []
  where
    go acc Empty = acc
    go acc (Combine a b) = go (go acc a) b
    go acc (MockSpec matcher ret) = (matcher, ret) : acc
