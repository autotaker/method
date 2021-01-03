{-# LANGUAGE LambdaCase #-}

module Test.Method.MonitorSpec where

import Control.Method
import RIO (newIORef, readIORef, throwString, void, writeIORef)
import Test.Hspec
import Test.Method.Matcher
import Test.Method.Monitor

spec :: Spec
spec = do
  describe "newClock" $ do
    it "is initialized with zero" $ do
      clock <- newClock
      readIORef clock `shouldReturn` Tick 0
  describe "newMonitor" $ do
    it "has empty trace" $ do
      clock <- newClock
      m <- newMonitor clock
      length <$> getEventLog m `shouldReturn` 0
    it "has given clock" $ do
      clock <- newClock
      m <- newMonitor clock
      ShowType (monitorClock m) `shouldBe` (ShowType clock :: ShowType Clock)

  describe "tick" $ do
    it "increments monitor's clock" $ do
      clock <- newClock
      m <- newMonitor clock
      t <- tick m
      t `shouldBe` Tick 0
      t1 <- tick m
      t1 `shouldBe` Tick 1

  describe "monitor" $ do
    let setup = do
          clock <- newClock
          monitor clock method'
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
  describe "interleave" $ do
    let setup = do
          clock <- newClock
          r <- newIORef (0 :: Int)
          let get = readIORef r
              put n = writeIORef r n
          (,) <$> monitor clock get <*> monitor clock put
    before setup $ do
      it "logs two method call" $ \((mGet, get), (mPut, put)) -> do
        put 10
        _ <- get
        _ <- get
        l1 <- getEventLog mGet
        l2 <- getEventLog mPut
        interleave l1 l2
          `shouldBe` [ Enter (Tick 0) (Right (10 :* Nil)),
                       Leave (Tick 1) (Tick 0) (Right (Right ())),
                       Enter (Tick 2) (Left Nil),
                       Leave (Tick 3) (Tick 2) (Left (Right 10)),
                       Enter (Tick 4) (Left Nil),
                       Leave (Tick 5) (Tick 4) (Left (Right 10))
                     ]
  describe "times" $ do
    let setup = do
          clock <- newClock
          let method' :: String -> IO ()
              method' _ = pure ()
          (m, method) <- monitor clock method'
          method "hoge"
          method "piyo"
          getEventLog m

    before setup $ do
      it "should call method twice" $ \logs -> do
        logs `shouldSatisfy` ((== 2) `times` call anything)

      it "should not call method with \"fuga\"" $ \logs -> do
        logs `shouldSatisfy` ((== 0) `times` call (when (== "fuga")))

      it "should call method with \"hoge\" at least once" $ \logs -> do
        logs `shouldSatisfy` ((>= 1) `times` call (when (== "hoge")))
