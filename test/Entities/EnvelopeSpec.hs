module Entities.EnvelopeSpec (spec) where

import Data.Aeson (decode, encode)
import Data.Wrapper.NonEmpty (unsafeNonEmpty)
import Entities.Envelope (Envelope, balance, mkEnvelope, name, toNothingIfEmpty, unsafeEnvelope)
import Lens.Micro
import Test.Hspec
import Test.QuickCheck
import ValueObjects.Money (unAmount, unsafeMoney)

spec :: Spec
spec = do
  describe "introduction" $ do
    describe "mkEnvelope" $ do
      it "constructs an Envelope instance" $
        property $ \txt mon ->
          mkEnvelope txt mon `shouldBe` Right (unsafeEnvelope txt mon)

    describe "parseJSON" $ do
      it "constructs an Envelope instance" $
        (decode "{ \"name\": \"fun\", \"balance\": \"1.5k eur\" }" :: Maybe Envelope)
          `shouldBe` Just (unsafeEnvelope (unsafeNonEmpty "fun") $ unsafeMoney 1500 "eur")

  describe "elimination" $ do
    it "can be elminated to name" $
      property $ \name' balance' ->
        unsafeEnvelope name' balance' ^. name `shouldBe` name'

    it "can be elminated to balance" $
      property $ \name' balance' ->
        unsafeEnvelope name' balance' ^. balance `shouldBe` balance'

    describe "toJSON" $ do
      it "eliminates a money instance" $
        encode (unsafeEnvelope (unsafeNonEmpty "fun") $ unsafeMoney 1500 "eur")
          `shouldBe` "{\"balance\":\"1.50k eur\",\"name\":\"fun\"}"

    describe "toNothingIfEmpty" $ do
      it "Money 0 -> Nothing" $ toNothingIfEmpty (unsafeEnvelope (unsafeNonEmpty "fun") $ unsafeMoney 0 "eur") `shouldBe` Nothing
      it "non zero Money -> Just" $
        property $ \mon ->
          ((/= 0) $ unAmount mon)
            ==> let en = unsafeEnvelope (unsafeNonEmpty "fun") mon
                 in toNothingIfEmpty en `shouldBe` Just en

  describe "elimination -> introduction" $ do
    it "toJSON -> fromJSON" $
      property $ \envelope ->
        (decode (encode envelope) :: Maybe Envelope) `shouldBe` pure envelope

  describe "Semigroup" $ do
    it "concats balances" $
      property $ \env1 env2 ->
        (env1 <> env2) ^. balance `shouldBe` (env1 ^. balance) <> (env2 ^. balance)

    describe "concats names" $ do
      it "concats names with and" $
        property $ \n1 n2 b1 b2 ->
          let env1 = unsafeEnvelope n1 b1
              env2 = unsafeEnvelope n2 b2
           in (env1 <> env2) ^. name `shouldBe` (env1 ^. name) <> (unsafeNonEmpty " and ") <> (env2 ^. name)
