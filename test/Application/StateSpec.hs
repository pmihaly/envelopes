module Application.StateSpec (spec) where

import Application.State (State (..), appliedTransactions, envelopes)
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Wrapper.NonEmpty (unsafeNonEmpty)
import Entities.Envelope (unsafeEnvelope)
import Lens.Micro
import Test.Hspec
import Test.QuickCheck
import ValueObjects.Id (unsafeId)
import ValueObjects.Money (unsafeMoney)
import ValueObjects.ShorthandNumber (ShorthandNumber (..))
import ValueObjects.Text50 (unsafeText50)

state :: State
state = State (Map.fromList [(unsafeId (unsafeNonEmpty $ unsafeText50 "fun"), unsafeEnvelope (unsafeNonEmpty "fun") $ unsafeMoney (ShorthandNumber 1500) "eur")]) (Set.fromList [unsafeId (unsafeNonEmpty $ unsafeText50 "already-applied")])

stateJson :: BSL.ByteString
stateJson = "{\"applied-transactions\":[\"already-applied\"],\"envelopes\":[{\"balance\":\"1.50k eur\",\"name\":\"fun\"}]}"

spec :: Spec
spec = do
  describe "introduction" $ do
    describe "fromJSON" $ do
      it "no envelopes key" $
        let noEnvelopeKeyJson :: BSL.ByteString
            noEnvelopeKeyJson = "{\"applied-transactions\":[\"already-applied\"]}"
         in (eitherDecode noEnvelopeKeyJson :: Either String State) `shouldBe` pure (envelopes .~ Map.empty $ state)

      it "no transactions key" $
        let noAppliedTxKeyJson :: BSL.ByteString
            noAppliedTxKeyJson = "{\"envelopes\":[{\"balance\":\"1.50k eur\",\"name\":\"fun\"}]}"
         in (eitherDecode noAppliedTxKeyJson :: Either String State) `shouldBe` pure (appliedTransactions .~ Set.empty $ state)

      it "duplicate envelope name" $
        let dupEnvNameJson :: BSL.ByteString
            dupEnvNameJson = "{\"applied-transactions\":[\"already-applied\"],\"envelopes\":[{\"balance\":\"77k eur\",\"name\":\"fun\"},{\"balance\":\"1.50k eur\",\"name\":\"fun\"}]}"
         in (eitherDecode dupEnvNameJson :: Either String State) `shouldBe` pure state

      it "duplicate applied transaction" $
        let dupEnvNameJson :: BSL.ByteString
            dupEnvNameJson = "{\"applied-transactions\":[\"already-applied\", \"already-applied\"],\"envelopes\":[{\"balance\":\"1.50k eur\",\"name\":\"fun\"}]}"
         in (eitherDecode dupEnvNameJson :: Either String State) `shouldBe` pure state

      it "happy path" $ (eitherDecode stateJson :: Either String State) `shouldBe` pure state

  describe "elimination" $ do
    it "toJSON" $ encode state `shouldBe` stateJson

  describe "elimination -> introduction" $ do
    it "toJSON -> fromJSON" $
      property $ \state' ->
        (eitherDecode (encode state') :: Either String State) `shouldBe` pure state'
