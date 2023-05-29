module Entities.TransactionSpec (spec) where

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.Wrapper.NonEmpty (unsafeNonEmpty)
import Data.Wrapper.Positive (unsafePositive)
import Entities.Transaction (Transaction (..))
import Test.Hspec
import Test.QuickCheck
import ValueObjects.Id (unsafeId)
import ValueObjects.Money (unsafeMoney)
import ValueObjects.ShorthandNumber (ShorthandNumber (..))
import ValueObjects.Text50 (unsafeText50)

spending :: Transaction
spending = Spending (unsafeId $ unsafeNonEmpty $ unsafeText50 "some-id") (unsafeId $ unsafeNonEmpty $ unsafeText50 "some-envelope") (unsafeNonEmpty $ unsafePositive $ unsafeMoney (ShorthandNumber 1500) "eur") "2023-12-14T00:00:00Z" ["tag-1"]

spendingJson :: BSL.ByteString
spendingJson = "{\"amount\":\"1.50k eur\",\"date\":\"2023-12-14T00:00:00Z\",\"envelope\":\"some-envelope\",\"id\":\"some-id\",\"tags\":[\"tag-1\"],\"type\":\"spending\"}"

spec :: Spec
spec = do
  describe "introduction" $ do
    describe "parseJSON" $ do
      it "constructs Spending" $
        (eitherDecode spendingJson :: Either String Transaction) `shouldBe` pure spending

  describe "elimination" $ do
    describe "toJSON" $ do
      it "eliminates Spending" $
        encode spending `shouldBe` spendingJson

  describe "elimination -> introduction" $ do
    it "toJSON -> fromJSON" $
      property $ \transaction ->
        (eitherDecode (encode transaction) :: Either String Transaction) `shouldBe` pure transaction
