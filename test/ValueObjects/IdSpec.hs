{-# LANGUAGE ImportQualifiedPost #-}

module ValueObjects.IdSpec (spec) where

import Control.Category ((>>>))
import Data.Aeson (decode, encode)
import Data.Text qualified as T
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TL
import Data.Wrapper.NonEmpty (NonEmptyError (..), unsafeNonEmpty)
import Test.Hspec
import Test.QuickCheck
import ValueObjects.Id (Id, IdError (..), mkId, unId, unsafeId)
import ValueObjects.Text50 (Text50Error (..), unsafeText50)

unsafeMkTestId :: T.Text -> Id a
unsafeMkTestId = unsafeText50 >>> unsafeNonEmpty >>> unsafeId

spec :: Spec
spec = do
  describe "introduction" $ do
    describe "mkId" $ do
      it "returns InvalidEmpty if the input is empty" $
        do
          mkId "" `shouldBe` Left (InvalidEmpty EmptyNotAllowed)

      it "returns TooLong if the input is more than 50 characters" $
        do
          mkId (T.pack $ Prelude.take 51 ['A' ..]) `shouldBe` Left (InvalidText TooLong)

      it "constructs the string with medium length input" $
        do
          mkId "some-text" `shouldBe` Right (unsafeMkTestId "some-text")

    describe "parseJSON" $ do
      it "returns Nothing if the input is empty" $
        do (decode "\"\"" :: Maybe (Id ())) `shouldBe` Nothing

      it "returns Nothing if the input is more than 50 characters" $
        do (decode ("\"" <> TL.encodeUtf8 (TL.pack $ Prelude.take 51 ['A' ..]) <> "\"") :: Maybe (Id ())) `shouldBe` Nothing

      it "constructs the string with medium length input" $
        do (decode "\"some-text\"" :: Maybe (Id ())) `shouldBe` Just (unsafeMkTestId "some-text")

      it "parseJSON can parse the output of toJSON" $
        property $
          \t -> Just t `shouldBe` (decode (encode t) :: Maybe (Id ()))

  describe "elimination" $ do
    it "can be converted to T.Text using unId" $
      do unId (unsafeMkTestId "some-text") `shouldBe` (unsafeNonEmpty $ unsafeText50 "some-text")

    it "can be converted to String using show" $
      do show (unsafeMkTestId "some-text") `shouldBe` "\"some-text\""

    it "can be converted to JSON using toJSON" $
      do encode (unsafeMkTestId "some-text") `shouldBe` "\"some-text\""
