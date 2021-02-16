module Test.Method.DynamicSpec where

import Control.Exception (evaluate)
import Data.Typeable
import Test.Hspec
import Test.Method.Dynamic
import Test.Method.Matcher
import Test.Method.Mock

spec :: Spec
spec = do
  describe "castMethod" $ do
    let f :: (Typeable a, Show a) => String -> a -> IO a
        f = castMethod f'
        f' :: String -> DynamicShow -> IO DynamicShow
        f' = mockup $ do
          when (args ((== "int"), dynArg (== (10 :: Int)))) `thenReturn` toDynShow (20 :: Int)
          when (args ((== "bool"), dynArg (== True))) `thenReturn` toDynShow False
          when (args ((== "invalid"), dynArg (anything :: Matcher Bool))) `thenReturn` toDynShow 'A'
          throwNoStub (when anything)
    it "polymorphic" $ do
      f "int" (10 :: Int) `shouldReturn` 20
      f "bool" True `shouldReturn` False
      (f "invalid" True >>= evaluate) `shouldThrow` anyErrorCall
      f "invalid" 'a' `shouldThrow` anyErrorCall
