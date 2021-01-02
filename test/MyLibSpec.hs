module MyLibSpec where

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "trivial" $
    it "True is True" $ True `shouldBe` True
