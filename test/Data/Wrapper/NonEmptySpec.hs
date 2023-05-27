{-# LANGUAGE ImportQualifiedPost #-}

module Data.Wrapper.NonEmptySpec (spec) where

import Data.Aeson (decode, encode)
import Data.Text qualified as T
import Data.Wrapper.NonEmpty (NonEmpty, NonEmptyError (..), mkNonEmpty, unNonEmpty, unsafeNonEmpty)
import Test.Hspec
import Test.QuickCheck
import ValueObjects.Text50 (Text50, unsafeText50)

spec :: Spec
spec = do
  describe "introduction" $ do
    describe "mkNonEmpty" $ do
      it "returns left if the input is empty" $
        do
          mkNonEmpty ("" :: T.Text) `shouldBe` Left EmptyNotAllowed

      it "constructs if the input is non-empty" $
        property $ \input ->
          not (null (input :: [Int])) ==> mkNonEmpty input `shouldBe` Right (unsafeNonEmpty input)

    describe "parseJSON" $ do
      it "returns left if input is empty" $
        do (decode "[]" :: Maybe (NonEmpty [Int])) `shouldBe` Nothing

      it "constructs if input is non-empty" $
        do (decode "\"abc\"" :: Maybe (NonEmpty Text50)) `shouldBe` Just (unsafeNonEmpty (unsafeText50 "abc"))

  describe "elimination" $ do
    it "can be eliminated using unNonEmpty" $
      property $ \input ->
        unNonEmpty (unsafeNonEmpty input :: NonEmpty [Char]) `shouldBe` input

    it "can be eliminated to String using show" $
      property $ \input ->
        show (unsafeNonEmpty input :: NonEmpty [Int]) `shouldBe` show input

    it "can be elimintated to JSON using toJSON" $
      do encode (unsafeNonEmpty "some-text" :: NonEmpty [Char]) `shouldBe` "\"some-text\""
