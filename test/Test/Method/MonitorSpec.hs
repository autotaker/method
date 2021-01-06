{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}

module Test.Method.MonitorSpec where

import Control.Method (TupleLike (fromTuple))
import RIO (newIORef, readIORef, throwString, void, writeIORef)
import Test.Hspec
  ( Spec,
    anyException,
    before,
    describe,
    it,
    shouldBe,
    shouldReturn,
    shouldSatisfy,
    shouldThrow,
  )
import Test.Method.Matcher (ArgsMatcher (args), anything)
import Test.Method.Monitor
  ( call,
    listenEventLog,
    newMonitor,
    times,
    watch,
    watchBy,
    withMonitor_,
  )
import Test.Method.Monitor.Internal
  ( Event (Enter, Leave),
    Monitor (monitorClock),
    Tick (Tick),
    tick,
  )

spec :: Spec
spec = do
  describe "newClock" $ do
    describe "newMonitor" $ do
      it "has empty trace" $ do
        m <- newMonitor
        length <$> listenEventLog m `shouldReturn` 0
      it "has clock initialized with zero" $ do
        m <- newMonitor
        readIORef (monitorClock m) `shouldReturn` Tick 0

    describe "tick" $ do
      it "increments monitor's clock" $ do
        m <- newMonitor
        t <- tick m
        t `shouldBe` Tick 0
        t1 <- tick m
        t1 `shouldBe` Tick 1

    describe "watch'" $ do
      let setup = do
            m <- newMonitor
            let method = watch m method'
            pure (m, method)
          method' :: Int -> IO String
          method' n
            | n == 42 = throwString "error"
            | otherwise = pure $ show n

      before setup $ do
        it "logs single method call" $ \(m, method) -> do
          void $ method 1
          listenEventLog m
            `shouldReturn` [ Enter (Tick 0) (fromTuple 1),
                             Leave (Tick 1) (Tick 0) (Right "1")
                           ]
        it "logs exception thrown" $ \(m, method) -> do
          method 42 `shouldThrow` anyException
          logs <- listenEventLog m
          shouldSatisfy (logs !! 1) $ \case
            Leave (Tick 1) (Tick 0) (Left _) -> True
            _ -> False
    describe "watch" $ do
      let setup = do
            m <- newMonitor
            r <- newIORef (0 :: Int)
            let get :: IO Int
                get = watchBy Left Left m (readIORef r)
                put :: Int -> IO ()
                put = watchBy Right Right m (writeIORef r)
            pure (m, get, put)
      before setup $ do
        it "logs two method call" $ \(m, get, put) -> do
          put 10
          _ <- get
          _ <- get
          listenEventLog m
            `shouldReturn` [ Enter (Tick 0) (Right (fromTuple 10)),
                             Leave (Tick 1) (Tick 0) (Right (Right ())),
                             Enter (Tick 2) (Left (fromTuple ())),
                             Leave (Tick 3) (Tick 2) (Right (Left 10)),
                             Enter (Tick 4) (Left (fromTuple ())),
                             Leave (Tick 5) (Tick 4) (Right (Left 10))
                           ]
    describe "times" $ do
      let setup = withMonitor_ $ \m -> do
            let method :: String -> IO ()
                method = watch m (const $ pure ())
            method "hoge"
            method "piyo"

      before setup $ do
        it "should call method twice" $ \logs -> do
          logs `shouldSatisfy` ((== 2) `times` call anything)

        it "should not call method with \"fuga\"" $ \logs -> do
          logs `shouldSatisfy` ((== 0) `times` call (args (== "fuga")))

        it "should call method with \"hoge\" at least once" $ \logs -> do
          logs `shouldSatisfy` ((>= 1) `times` call (args (== "hoge")))
