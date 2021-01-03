module Test.Method.MockSpec where

import Control.Method ((:*) ((:*)))
import Test.Hspec (Selector, Spec, describe, it, shouldReturn, shouldThrow)
import Test.Method.Mock (NoStubException, ToMatcher (when), mockup, thenReturn)

spec :: Spec
spec = do
  describe "mockup" $ do
    let method :: Int -> Int -> IO Int
        method = mockup $ do
          when ((==) 1, (>=) 2) `thenReturn` 3
          (\(a :* b :* _) -> a * b == 6) `thenReturn` 42
    it "mock method 1 2 returns 3" $ do
      method 1 2 `shouldReturn` 3

    it "mock method 2 2 throws Exception" $ do
      method 0 2 `shouldThrow` anyNoStubException

    it "mock method 2 3 returns 42" $ do
      method 2 3 `shouldReturn` 42

anyNoStubException :: Selector NoStubException
anyNoStubException _ = True