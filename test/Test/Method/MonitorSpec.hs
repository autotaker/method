{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}

module Test.Method.MonitorSpec where

import Control.Method (Nil (Nil), type (:*) ((:*)))
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
import Test.Method.Matcher (ArgsTuple (args), anything)
import Test.Method.Monitor
  ( Event (Enter, Leave),
    Monitor (monitorClock),
    Tick (Tick),
    call,
    getEventLog,
    newMonitor,
    tick,
    times,
    watch,
    watch',
  )

spec :: Spec
spec = do
  describe "newClock" $ do
    describe "newMonitor" $ do
      it "has empty trace" $ do
        m <- newMonitor
        length <$> getEventLog m `shouldReturn` 0
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
            method <- watch' m method'
            pure (m, method)
          method' :: Int -> IO String
          method' n
            | n == 42 = throwString "error"
            | otherwise = pure $ show n

      before setup $ do
        it "logs single method call" $ \(m, method) -> do
          void $ method 1
          getEventLog m
            `shouldReturn` [ Enter (Tick 0) (1 :* Nil),
                             Leave (Tick 1) (Tick 0) (Right "1")
                           ]
        it "logs exception thrown" $ \(m, method) -> do
          method 42 `shouldThrow` anyException
          logs <- getEventLog m
          shouldSatisfy (logs !! 1) $ \case
            Leave (Tick 1) (Tick 0) (Left _) -> True
            _ -> False
    describe "watch" $ do
      let setup = do
            m <- newMonitor
            r <- newIORef (0 :: Int)
            let get = readIORef r
                put n = writeIORef r n
            get' <- watch Left Left m get
            put' <- watch Right Right m put
            pure (m, get', put')
      before setup $ do
        it "logs two method call" $ \(m, get, put) -> do
          put 10
          _ <- get
          _ <- get
          getEventLog m
            `shouldReturn` [ Enter (Tick 0) (Right (10 :* Nil)),
                             Leave (Tick 1) (Tick 0) (Right (Right ())),
                             Enter (Tick 2) (Left Nil),
                             Leave (Tick 3) (Tick 2) (Right (Left 10)),
                             Enter (Tick 4) (Left Nil),
                             Leave (Tick 5) (Tick 4) (Right (Left 10))
                           ]
    describe "times" $ do
      let setup = do
            m <- newMonitor
            let method' :: String -> IO ()
                method' _ = pure ()
            method <- watch' m method'
            method "hoge"
            method "piyo"
            getEventLog m

      before setup $ do
        it "should call method twice" $ \logs -> do
          logs `shouldSatisfy` ((== 2) `times` call anything)

        it "should not call method with \"fuga\"" $ \logs -> do
          logs `shouldSatisfy` ((== 0) `times` call (args (== "fuga")))

        it "should call method with \"hoge\" at least once" $ \logs -> do
          logs `shouldSatisfy` ((>= 1) `times` call (args (== "hoge")))
