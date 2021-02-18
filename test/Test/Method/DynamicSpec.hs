{-# LANGUAGE TypeApplications #-}

module Test.Method.DynamicSpec where

import Control.Exception (evaluate)
import Control.Exception.Base (ErrorCall (ErrorCall))
import Data.Proxy (Proxy (Proxy), asProxyTypeOf)
import Data.Typeable (Typeable)
import Test.Hspec
  ( Expectation,
    Spec,
    anyErrorCall,
    describe,
    it,
    shouldBe,
    shouldReturn,
    shouldThrow,
  )
import Test.Method.Dynamic
  ( Dynamic,
    DynamicShow,
    FromDyn (fromDyn),
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
  describe "fromDyn" $ do
    let fid :: (FromDyn b a, ToDyn b a, Show a, Eq a) => Proxy b -> a -> Expectation
        fid proxy x = fromDyn (toDyn x `asProxyTypeOf` proxy) `shouldBe` x
    it "Int -> Int" $ fid (Proxy @Int) (0 :: Int)
    it "Dynamic -> Int" $ fid (Proxy @Dynamic) (0 :: Int)
    it "DynamicShow ->Int" $ fid (Proxy @DynamicShow) (0 :: Int)
    it "[Dynamic] -> [Int]" $ fid (Proxy @[Dynamic]) [0 :: Int]
    it "Maybe Dynamic -> Maybe ()" $ fid (Proxy @(Maybe Dynamic)) (Nothing :: Maybe ())
    it "Either Dynamic Bool -> Either Int Bool" $ fid (Proxy @(Either Dynamic Bool)) (Left 0 :: Either Int Bool)
    it "(Int, Dynamic) -> (Int, Int)" $ fid (Proxy @(Int, Dynamic)) (0 :: Int, 0 :: Int)
    it "Dynamic^7 -> Char^7" $
      fid
        (Proxy @(Dynamic, Dynamic, Dynamic, Dynamic, Dynamic, Dynamic, Dynamic))
        ('a', 'b', 'c', 'd', 'e', 'f', 'g')
    it "((Int, Dynamic) -> (Dynamic, Int)) -> ((Int, Int) -> (Int, Int))" $ do
      let f :: (Int, Int) -> (Int, Int)
          f = fromDyn (toDyn (id :: (Int, Int) -> (Int, Int)) :: ((Int, Dynamic) -> (Dynamic, Int)))
      f (42, 57) `shouldBe` (42, 57)

    it "throw type error" $
      evaluate (fromDyn (toDyn True :: Dynamic) :: Char)
        `shouldThrow` ( \(ErrorCall msg) ->
                          msg == "cannot cast <<Bool>> to Char"
                      )

  describe "Show DynamicShow" $ do
    it "show (DynamicShow (0 :: Int)) == \"<<0 :: Int>>\"" $ do
      show (toDyn (0 :: Int) :: DynamicShow) `shouldBe` "<<0 :: Int>>"
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
