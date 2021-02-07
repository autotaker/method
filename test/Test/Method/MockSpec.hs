module Test.Method.MockSpec where

import RIO.Writer (MonadWriter (tell))
import Test.Hspec (Spec, anyErrorCall, describe, it, shouldReturn, shouldThrow)
import Test.Method.Matcher (ArgsMatcher (args), args', when)
import Test.Method.Mock (mockup, thenReturn)

spec :: Spec
spec = do
  describe "mockup" $ do
    let method :: Int -> Int -> IO Int
        method = mockup $ do
          tell $ when (args ((==) 1, (>=) 2)) `thenReturn` 3
          tell $ when (args' (\(a, b) -> a * b == 6)) `thenReturn` 42
    it "mock method 1 2 returns 3" $ do
      method 1 2 `shouldReturn` 3

    it "mock method 2 2 throws ErrorCall" $ do
      method 0 2 `shouldThrow` anyErrorCall

    it "mock method 2 3 returns 42" $ do
      method 2 3 `shouldReturn` 42
