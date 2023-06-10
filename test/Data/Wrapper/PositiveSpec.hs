{-# LANGUAGE ImportQualifiedPost #-}

module Data.Wrapper.PositiveSpec (spec) where

import Data.Aeson (decode, encode)
import Data.Wrapper.Positive (PositiveError (..), mkPositive, unPositive, unsafePositive)
import Data.Wrapper.Positive qualified as P
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "introduction" $ do
    describe "mkPositive" $ do
      it "returns left if the input is negative" $
        property $ \(n :: Float) ->
          n < 0.0 ==>
            mkPositive n `shouldBe` Left NegativeNotAllowed

      it "constructs if the input is positive" $
        property $ \(n :: Float) ->
          n >= 0.0 ==>
            mkPositive n `shouldBe` Right (unsafePositive n)

    describe "parseJSON" $ do
      it "returns left if input is negative" $
        do (decode "-2" :: Maybe (P.Positive Int)) `shouldBe` Nothing

      it "constructs if input is positive" $
        do (decode "4" :: Maybe (P.Positive Int)) `shouldBe` Just (unsafePositive 4)

  describe "elimination" $ do
    it "can be eliminated using unPositive" $
      property $ \input ->
        unPositive (unsafePositive input :: P.Positive Float) `shouldBe` input

    it "can be eliminated to String using show" $
      property $ \input ->
        show (unsafePositive input :: P.Positive Float) `shouldBe` show input

    it "can be elimintated to JSON using toJSON" $
      do encode (unsafePositive 23.4 :: P.Positive Float) `shouldBe` "23.4"
