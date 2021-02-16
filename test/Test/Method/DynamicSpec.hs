module Test.Method.DynamicSpec where

import Control.Exception (evaluate)
import Data.Typeable (Typeable)
import Test.Hspec
  ( Spec,
    anyErrorCall,
    describe,
    it,
    shouldReturn,
    shouldThrow,
  )
import Test.Method.Dynamic
  ( DynamicShow,
    ToDyn (toDyn),
    castMethod,
    dynArg,
  )
import Test.Method.Matcher
  ( ArgsMatcher (args),
    Matcher,
    anything,
    when,
  )
import Test.Method.Mock (mockup, thenReturn, throwNoStub)

spec :: Spec
spec = do
  describe "castMethod" $ do
    let f :: (Typeable a, Show a) => String -> a -> IO a
        f = castMethod f'
        f' :: String -> DynamicShow -> IO DynamicShow
        f' = mockup $ do
          when (args ((== "int"), dynArg (== (10 :: Int)))) `thenReturn` toDyn (20 :: Int)
          when (args ((== "bool"), dynArg (== True))) `thenReturn` toDyn False
          when (args ((== "invalid"), dynArg (anything :: Matcher Bool))) `thenReturn` toDyn 'A'
          throwNoStub (when anything)
    it "polymorphic" $ do
      f "int" (10 :: Int) `shouldReturn` 20
      f "bool" True `shouldReturn` False
      (f "invalid" True >>= evaluate) `shouldThrow` anyErrorCall
      f "invalid" 'a' `shouldThrow` anyErrorCall
