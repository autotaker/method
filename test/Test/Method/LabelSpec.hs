{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

--{-# OPTIONS_GHC -ddump-splices -ddump-simpl -ddump-to-file #-}

module Test.Method.LabelSpec where

import Data.Typeable (typeRep)
import RIO
  ( Display (textDisplay),
    Proxy (Proxy),
    RIO,
    Text,
    Typeable,
    displayShow,
    runRIO,
  )
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import Test.Method.Label
  ( Label (toInterface),
    deriveLabel,
    type (:|:) (L, R),
  )

data API env = API
  { _foo :: RIO env Int,
    _bar :: Text -> RIO env Text,
    _baz :: forall a. Typeable a => (Int, Bool) -> RIO env [a]
  }

deriveLabel ''API

data FizzBuzz env = FizzBuzz
  { _fizz :: RIO env Text,
    _buzz :: RIO env Text,
    _others :: forall a. (Show a, Typeable a) => a -> RIO env Text
  }

deriveLabel ''FizzBuzz

spec :: Spec
spec = do
  describe "deriveLabel ''API" $ do
    describe "APILabel" $ do
      it "Foo is defined" $ do
        typeRep (Foo @()) `shouldBe` typeRep (Proxy :: Proxy (RIO () Int))
      it "Bar is defined" $ do
        typeRep (Bar @()) `shouldBe` typeRep (Proxy :: Proxy (Text -> RIO () Text))
    describe "toRecord" $ do
      let api = toInterface mock
      it "_foo api returns 0" $ do
        runRIO () (_foo api) `shouldReturn` 0
      it "_bar api \"hello\" returns \"hello hoge\"" $ do
        runRIO () (_bar api "hello") `shouldReturn` "hello hoge"
  describe "(:|:)" $ do
    let (api, fizzbuzz) = toInterface mock2
    it "_foo api returns 1" $ do
      runRIO () (_foo api) `shouldReturn` 1
    it "_fizz fizzbuzz returns \"Fizz\"" $ do
      runRIO () (_fizz fizzbuzz) `shouldReturn` "Fizz"
    it "_others fizzbuzz 10 returns \"<<10 :: Int>>\"" $ do
      runRIO () (_others fizzbuzz (10 :: Int)) `shouldReturn` "<<10 :: Int>>"

mock :: APILabel env m -> m
mock Foo = pure 0
mock Bar = \x -> pure (x <> " hoge")
mock Baz = undefined

mock2 :: (:|:) (APILabel env) (FizzBuzzLabel env) m -> m
mock2 (L Foo) = pure 1
mock2 (L Bar) = \x -> pure (x <> " bar")
mock2 (L Baz) = undefined
mock2 (R Fizz) = pure "Fizz"
mock2 (R Buzz) = pure "Buzz"
mock2 (R Others) = pure . textDisplay . displayShow