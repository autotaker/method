{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Method.ProtocolSpec where

import RIO (ByteString, Text, void)
import Test.Hspec
  ( Spec,
    anyErrorCall,
    before,
    context,
    describe,
    it,
    shouldReturn,
    shouldThrow,
  )
import Test.Method.Dynamic (ToDyn (toDyn), Typeable, dynArg)
import Test.Method.Label (deriveLabel)
import Test.Method.Protocol
  ( ProtocolM,
    decl,
    dependsOn,
    mockInterface,
    protocol,
    thenReturn,
    verify,
    whenArgs,
    withProtocol,
  )

type UserName = Text

type PlainPassword = ByteString

type HashedPassword = ByteString

type UserId = Int

data Env = Env

data User = User {userName :: UserName, userId :: UserId}
  deriving (Eq, Ord, Show)

data Service = Service
  { findUser :: UserName -> IO (Maybe User),
    findPassword :: UserName -> IO (Maybe HashedPassword),
    createUser :: UserName -> IO User,
    hashPassword :: PlainPassword -> IO HashedPassword,
    upsertAuth :: UserId -> HashedPassword -> IO ()
  }

deriveLabel ''Service

doService :: Service -> UserName -> PlainPassword -> IO (Maybe User)
doService Service {..} usernm passwd = do
  mUser <- findUser usernm
  case mUser of
    Just _ -> pure Nothing
    Nothing -> do
      hpasswd <- hashPassword passwd
      user <- createUser usernm
      upsertAuth (userId user) hpasswd
      pure $ Just user

doServiceHashFirst :: Service -> UserName -> PlainPassword -> IO (Maybe User)
doServiceHashFirst Service {..} usernm passwd = do
  hpasswd <- hashPassword passwd
  mUser <- findUser usernm
  case mUser of
    Just _ -> pure Nothing
    Nothing -> do
      user <- createUser usernm
      upsertAuth (userId user) hpasswd
      pure $ Just user

doServiceWrongArgument :: Service -> UserName -> PlainPassword -> IO (Maybe User)
doServiceWrongArgument Service {..} usernm passwd = do
  mUser <- findUser usernm
  case mUser of
    Just _ -> pure Nothing
    Nothing -> do
      _ <- hashPassword passwd
      user <- createUser usernm
      upsertAuth (userId user) passwd
      pure $ Just user

doServiceWrongExtraCalls :: Service -> UserName -> PlainPassword -> IO (Maybe User)
doServiceWrongExtraCalls Service {..} usernm passwd = do
  mUser <- findUser usernm
  case mUser of
    Just _ -> pure Nothing
    Nothing -> do
      hpasswd <- hashPassword passwd
      hpasswd' <- hashPassword hpasswd
      user <- createUser usernm
      upsertAuth (userId user) hpasswd'
      pure $ Just user

doServiceWrongMissingCalls :: Service -> UserName -> PlainPassword -> IO (Maybe User)
doServiceWrongMissingCalls Service {..} usernm passwd = do
  mUser <- findUser usernm
  case mUser of
    Just _ -> pure Nothing
    Nothing -> do
      _ <- hashPassword passwd
      user <- createUser usernm
      pure $ Just user

doServiceWrongOrder :: Service -> UserName -> PlainPassword -> IO (Maybe User)
doServiceWrongOrder Service {..} usernm passwd = do
  user <- createUser usernm
  mUser <- findUser usernm
  case mUser of
    Just _ -> pure Nothing
    Nothing -> do
      hpasswd <- hashPassword passwd
      upsertAuth (userId user) hpasswd
      pure $ Just user

doServiceWrongUnspecifiedMethod :: Service -> UserName -> PlainPassword -> IO (Maybe User)
doServiceWrongUnspecifiedMethod Service {..} usernm _passwd = do
  mUser <- findUser usernm
  case mUser of
    Just _ -> pure Nothing
    Nothing -> do
      Just hpasswd <- findPassword usernm
      user <- createUser usernm
      upsertAuth (userId user) hpasswd
      pure $ Just user

serviceProtocol :: UserName -> PlainPassword -> HashedPassword -> UserId -> ProtocolM ServiceLabel ()
serviceProtocol usernm passwd hpasswd userid = do
  i1 <- decl $ whenArgs FindUser (== usernm) `thenReturn` Nothing
  i2 <- decl $ whenArgs CreateUser (== usernm) `thenReturn` User usernm userid `dependsOn` [i1]
  i3 <- decl $ whenArgs HashPassword (== passwd) `thenReturn` hpasswd
  void $ decl $ whenArgs UpsertAuth ((== userid), (== hpasswd)) `thenReturn` () `dependsOn` [i2, i3]

data DBService = DBService
  { query :: forall a. (Typeable a, Show a) => String -> IO [a],
    execute :: forall a. (Typeable a, Show a) => String -> a -> IO ()
  }

deriveLabel ''DBService

dbProtocol :: ProtocolM DBServiceLabel ()
dbProtocol = do
  i1 <-
    decl $
      whenArgs Query (== "SELECT user_id FROM user")
        `thenReturn` toDyn [1, 2, 3 :: Int]
  _ <-
    decl $
      whenArgs Execute ((== "INSERT INTO friend(user_id, other_id) VALUES (?,?)"), dynArg (== (1 :: Int, 2 :: Int)))
        `thenReturn` () `dependsOn` [i1]
  pure ()

doDBService :: DBService -> IO ()
doDBService DBService {..} = do
  (user1 : user2 : _) <- query "SELECT user_id FROM user"
  execute "INSERT INTO friend(user_id, other_id) VALUES (?,?)" (user1, user2 :: Int)

spec :: Spec
spec = describe "protocol" $ do
  let usernm = "user1"
      passwd = "password1"
      hpasswd = "hashed_password1"
      userid = 0
  let setup = do
        penv <- protocol $ serviceProtocol usernm passwd hpasswd userid
        pure (penv, mockInterface penv)
  before setup $ do
    context "accept valid impl" $ do
      it "accept valid impl findUser first" $ \(penv, service) -> do
        doService service usernm passwd `shouldReturn` Just (User usernm userid)
        verify penv
      it "accept valid impl hash first" $ \(penv, service) -> do
        doServiceHashFirst service usernm passwd `shouldReturn` Just (User usernm userid)
        verify penv
    context "reject invalid impl" $ do
      it "invalid argument" $ \(_, service) ->
        doServiceWrongArgument service usernm passwd `shouldThrow` anyErrorCall
      it "extra calls" $ \(_, service) ->
        doServiceWrongExtraCalls service usernm passwd `shouldThrow` anyErrorCall
      it "missing calls" $ \(penv, service) -> do
        doServiceWrongMissingCalls service usernm passwd `shouldReturn` Just (User usernm userid)
        verify penv `shouldThrow` anyErrorCall
      it "wrong order" $ \(_, service) -> do
        doServiceWrongOrder service usernm passwd `shouldThrow` anyErrorCall
      it "unspecified method call" $ \(_, service) -> do
        doServiceWrongUnspecifiedMethod service usernm passwd `shouldThrow` anyErrorCall
  context "dbservice" $ do
    it "mock polymorphic interface" $ do
      withProtocol dbProtocol $ \svc ->
        doDBService svc `shouldReturn` ()
