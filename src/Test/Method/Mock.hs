{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Method.Mock where

import Control.Method
  ( Method (Args, Base, Ret, curryMethod),
    Nil (Nil),
    type (:*) ((:*)),
  )
import Data.Data (Typeable)
import RIO (Exception, MonadThrow (throwM))
import RIO.List (find)
import RIO.Writer (MonadWriter (tell), Writer, execWriter)

type Mock method = Writer (MockSpec method) ()

data MockSpec method
  = Empty
  | Combine (MockSpec method) (MockSpec method)
  | MockSpec (Matcher (Args method)) (Base method (Ret method))

newtype NoStubException = NoStubException String
  deriving (Show, Typeable)

instance Exception NoStubException

type Matcher a = a -> Bool

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

class ToMatcher a where
  type MatchAs a
  when :: a -> Matcher (MatchAs a)

instance ToMatcher (Matcher a) where
  type MatchAs (Matcher a) = a :* Nil
  when ma (a :* Nil) = ma a

instance ToMatcher (Matcher a, Matcher b) where
  type MatchAs (Matcher a, Matcher b) = a :* b :* Nil
  when (ma, mb) (a :* b :* Nil) = ma a && mb b

instance ToMatcher (Matcher a, Matcher b, Matcher c) where
  type MatchAs (Matcher a, Matcher b, Matcher c) = a :* b :* c :* Nil
  when (ma, mb, mc) (a :* b :* c :* Nil) = ma a && mb b && mc c

instance ToMatcher (Matcher a, Matcher b, Matcher c, Matcher d) where
  type MatchAs (Matcher a, Matcher b, Matcher c, Matcher d) = a :* b :* c :* d :* Nil
  when (ma, mb, mc, md) (a :* b :* c :* d :* Nil) = ma a && mb b && mc c && md d

instance ToMatcher (Matcher a, Matcher b, Matcher c, Matcher d, Matcher e) where
  type MatchAs (Matcher a, Matcher b, Matcher c, Matcher d, Matcher e) = a :* b :* c :* d :* e :* Nil
  when (ma, mb, mc, md, me) (a :* b :* c :* d :* e :* Nil) = ma a && mb b && mc c && md d && me e

instance ToMatcher (Matcher a, Matcher b, Matcher c, Matcher d, Matcher e, Matcher f) where
  type MatchAs (Matcher a, Matcher b, Matcher c, Matcher d, Matcher e, Matcher f) = a :* b :* c :* d :* e :* f :* Nil
  when (ma, mb, mc, md, me, mf) (a :* b :* c :* d :* e :* f :* Nil) = ma a && mb b && mc c && md d && me e && mf f

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
