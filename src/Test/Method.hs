{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Module : Test.Method
-- License: BSD-3
-- Maintainer: autotaker@gmail.com
-- Stability: experimental
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
    throwNoStubWithShow,
    throwNoStub,

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

    -- * Protocol

    -- ** Usage
    -- $protocol

    -- ** References
    protocol,
    ProtocolM,
    ProtocolEnv,
    CallId,
    decl,
    whenArgs,
    dependsOn,
    lookupMock,
    lookupMockWithShow,
    verify,

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

    -- * Mocking polymorphic methods

    -- ** Usage
    -- $dynamic
    DynamicShow,
    Dynamic,
    castMethod,
    dynArg,
    FromDyn (fromDyn),
    ToDyn (toDyn),
    Typeable,
  )
where

import Test.Method.Dynamic
import Test.Method.Matcher
  ( ArgsMatcher (..),
    Matcher,
    TupleLike (..),
    anything,
    args',
    when,
  )
import Test.Method.Mock
  ( mockup,
    thenAction,
    thenMethod,
    thenReturn,
    throwNoStub,
    throwNoStubWithShow,
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
import Test.Method.Protocol
  ( CallId,
    ProtocolEnv,
    ProtocolM,
    decl,
    dependsOn,
    lookupMock,
    lookupMockWithShow,
    protocol,
    verify,
    whenArgs,
  )

-- $usage
-- This module provides DSLs for mocking
-- methods and for validating method calls

-- $mock
--
-- @
-- fizzbuzz :: Int -> IO String
-- fizzbuzz = 'mockup' $ do
--   'when' ('args' (\\x -> mod x 15 == 0)) `'thenReturn'` "fizzbuzz"
--   'when' ('args' (\\x -> mod x 3 == 0)) `'thenReturn'` "fizz"
--   'when' ('args' (\\x -> mod x 5 == 0)) `'thenReturn'` "buzz"
--   'when' ('args' (>=0)) `'thenMethod'` (\\x -> pure $ show x)
--   'throwNoStub' $ 'when' 'anything'
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
-- *** Exception: no stub found for argument: -1
-- CallStack (from HasCallStack):
--  error, called at src/Test/Method/Mock.hs:98:9 in method-0.2.0.0-inplace:Test.Method.Mock"

-- @

-- $monitor
--
-- Production code
--
-- @
-- type ExampleMethod env = Int -> String -> RIO env ()
--
-- class HasExampleMethod env where
--   exampleL :: Lens\' env (ExampleMethod env)
--
-- doit :: HasExampleMethod env => RIO env ()
-- doit = (do
--   invoke exampleL 2 "foo"
--   invoke exampleL 3 "foo"
--   invoke exampleL (-1) "bar"
--   invoke exampleL 3 "bar") `catchAny` (const $ pure ())
-- @
--
-- Test code
--
-- @
-- data Env = Env { _example :: ExampleMethod env }
-- makeLenses Env''
--
-- instance HasExampleMethod Env where
--   exampleL = example
--
-- exampleMock :: ExampleMethod
-- exampleMock = 'mockup' $ do
--   'when' ('args' ((<0), 'anything')) `'thenAction'` throwString "negative n"
--   'when' 'anything' `'thenReturn'` ()
--
-- env = Env exampleMock
--
-- spec :: Spec
-- spec = describe "doit" $ do
--   before $ 'withMonitor_' $ \\monitor -> runRIO env $ local (exampleL %~ 'watch' monitor) doit
--
--   it "calls example _ \\\"foo\\\" twice" $ \\logs -> do
--     logs `'shouldSatisfy'` ((==2) `'times'` 'call' ('args' ('anything', (=="foo"))))
--
--   it "calls example (-1) \\\"bar\\\" once" $ \\logs -> do
--     logs `'shouldSatisfy'` ((==1) `'times'` 'call' ('args' ((==(-1)), (=="bar"))))
--
--   it "does not call example 3 \\\"bar\\\" " $ \\logs -> do
--     logs `'shouldSatisfy'` ((==0) `'times'` 'call' ('args' ((==3), (=="bar"))))
-- @

-- $protocol
-- Protocol is a DSL to write specification on communications between dependent methods.
-- By using Protocol, you can specify
--
-- * how many times each method is called,
-- * what arguments are passed for each call, and
-- * in which order methods are called.
--
-- For example, let's test user creation logic @signup@.
--
-- @
-- signup :: Service -> Username -> IO (Maybe UserId)
-- signup svc username = ...
-- type UserName = String
-- type UserId = Int
-- @
--
-- This method depends on @Service@, which consists of two methods.
--
-- * @findUser@: checks whether the user name is taken already,
-- * @createUser@: creates a user with given user name.
--
-- @
-- data Service = Service{
--   findUser :: UserName -> IO (Maybe UserId),
--   createUser :: UserName -> IO UserId
-- }
-- @
--
-- Let's check the following specification of @signup@ method.
--
-- 1. If @findUser@ returns @Just user@, it returns @Nothing@ without calling @createUser@.
-- 2. If @findUser@ returns @Nothing@, it calls @createUser@ and returns the created user.
--
-- In order to write Protocol DSL, first you define a GADT functor that
-- represents labels of dependent methods.
--
-- @
-- data Methods m where
--   FindUser :: Methods (UserName -> IO (Maybe UserId))
--   CreateUser :: Methods (UserName -> IO UserId)
--
-- deriving instance (Show (Methods m))
-- deriving instance (Eq (Methods m))
-- deriving instance (Ord (Methods m))
-- @
--
-- Then, you can write test for the specification.
--
-- @
-- spec :: Spec
-- spec = do
--   describe "signup" $ do
--     let username = "user1"
--         userId = 1
--     context "if ``findUser`` returns `Just user`" $
--       it "return \`Nothing\` without calling ``createUser``" $ do
--         -- Because env is stateful, it should be initialized for each test
--         env <- 'protocol' $ do
--           'decl' $ 'whenArgs' FindUser (==username) ``thenReturn`` Just userId
--         -- mocking methods from protocol env. Each mock method raises an exception
--         -- if it is called in a different way than that specified by the protocol.
--         let service = Service {
--               findUser = 'lookupMock' FindUser env,
--               createUser = 'lookupMock' CreateUser env
--             }
--         signup service username \`shouldReturn\` Nothing
--         -- Checks all calls specified by the protocol are called.
--         'verify' env
--
--       it "call ``createUser`` and return `Just userId`" $ do
--         env <- protocol $ do
--           findUserCall <- 'decl' $ 'whenArgs' FindUser (==username) ``thenReturn`` Nothing
--           'decl' $ 'whenArgs' CreateUser (==username) ``thenReturn`` Just userId ``dependsOn`` [findUserCall]
--         let service = Service {
--               findUser = 'lookupMock' FindUser env,
--               createUser = 'lookupMock' CreateUser env
--             }
--         signup service username ``shouldReturn`` Just userId
--         'verify' env
-- @
--
-- Protocol DSL consists of method call declarations like:
--
-- @
-- 'decl' $ 'whenArgs' FindUser (=="user1") ``thenReturn`` Nothing
-- @
--
-- This declaration specifies that @findUser@ is called once with argument @"user1"@
-- and it returns @Nothing@.
-- If @findUser@ is called with other argument, it raises an exception.
--
-- In protocol DSL, you can specify in which order methods are called, by using 'dependsOn' function.
-- For example:
--
-- @
-- findUserCall <- 'decl' $ 'whenArgs' FindUser (=="user1") ``thenReturn`` Nothing
-- 'decl' $ 'whenArgs' CreateUser (=="user1") ``thenReturn`` Nothing ``dependsOn`` [findUserCall]
-- @
--
-- @findUser@ must be called before calling @createUser@.
-- On the other hand, in the following example:
--
-- @
-- 'decl' $ 'whenArgs' FindUser (=="user1") ``thenReturn`` Nothing
-- 'decl' $ 'whenArgs' CreateUser (=="user1") ``thenReturn`` Nothing
-- @
--
-- the order of calling two methods does not matter.
--
-- However, each call declaration implicitly depends on the previous call declaration of the same method.
-- For example:
--
-- @
-- 'decl' $ 'whenArgs' FindUser (=="user1") ``thenReturn`` Nothing
-- 'decl' $ 'whenArgs' FindUser (=="user2") ``thenReturn`` Just 1
-- @
--
-- @findUser "user1"@ must be called before @findUser "user2"@ is called.

-- $dynamic
-- Often you want to mock polymorphic functions.
-- For example, assume that we are testing the following method.
--
-- @
-- type QueryFunc = forall q r. (ToRow q, FromRow r) => Query -> q -> IO [r]
-- service :: QueryFunc -> Day -> IO [Event]
-- service query today = do
--   events <- query "SELECT * FROM event WHERE date = ?" (Only today)
--   pure events
-- @
--
-- Because @QueryFunc@ is a polymorphic function, it is impossible to mock directly with 'mockup'.
--
-- In order to mock @QueryFunc@, first add 'Typeable' (and 'Show') constraint(s) for each type variables.
--
-- @
-- type QueryFunc = forall q r. (ToRow q, 'Typeable' q, 'Show' q, FromRow r, 'Typeable' r, 'Show' r) => Query -> q -> IO [r]
-- @
--
-- Then, we can mock dynamic version of 'QueryFunc', where each type variable is replaced with 'DynamicShow'
-- (or 'Dynamic'),
-- and obtain polymorphic method by casting
-- the dynamic version with 'castMethod'.
--
-- @
-- queryDyn :: Query -> 'DynamicShow' -> IO ['DynamicShow']
-- queryDyn = 'mockup' $ ...
-- queryMock :: QueryFunc
-- queryMock = 'castMethod' queryDyn
-- @
--
-- Then, you can write test for @service@ as follows.
--
-- @
-- spec :: Spec
-- spec = do
--   describe "service" $ do
--     it "return events whose date are today" $ do
--       let today = fromGregorian 2020 2 20
--           sql = "SELECT * FROM event WHERE date = ?"
--           events = [Event 0, Event 1]
--           queryDyn :: Query -> 'DynamicShow' -> IO ['DynamicShow']
--           queryDyn = mockup $
--             when (args ((==sql), 'dynArg' (==today))) ``thenReturn`` 'toDyn' events
--       service ('castMethod' queryDyn) today ``shouldReturn`` events
-- @
