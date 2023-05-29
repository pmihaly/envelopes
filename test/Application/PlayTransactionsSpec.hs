module Application.PlayTransactionsSpec (spec) where

import Application.PlayTransactions (playTransactions)
import Application.State (State (..), appliedTransactions)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Data.Wrapper.NonEmpty (unsafeNonEmpty)
import Data.Wrapper.Positive (unsafePositive)
import Entities.Envelope (unsafeEnvelope)
import Entities.Transaction (Transaction (..), getTransactionId)
import Lens.Micro.Platform
import Test.ArbitraryInstances (unArbitraryScientific)
import Test.Hspec
import Test.QuickCheck
import ValueObjects.Id (unsafeId)
import ValueObjects.Money (unsafeMoney)
import ValueObjects.ShorthandNumber (ShorthandNumber (..))
import ValueObjects.Text50 (unsafeText50)

spec :: Spec
spec = do
  it "should insert transactionId to appliedTransactions" $
    property $
      \state transaction ->
        (not $ (getTransactionId transaction) `elem` state ^. appliedTransactions)
          ==> (\state' -> getTransactionId transaction `elem` (state' ^. appliedTransactions)) <$> playTransactions state [transaction]
          `shouldBe` pure True

  describe "idempotence" $ do
    it "should not apply the same transaction twice" $
      property $
        withMaxSuccess 1000 $ \state transaction ->
          playTransactions state [transaction] `shouldBe` playTransactions state [transaction, transaction]

  describe "spending" $ do
    it "happy path" $
      property $ \as ->
        let amount = unArbitraryScientific as
            state = stateEnvelopeWith amount []
            expectedState = stateEnvelopeWith (amount - 1000) ["some-spending"]
         in playTransactions state [spending] `shouldBe` pure expectedState

    it "not existing envelope" $
      let state = State (Map.fromList []) Set.empty
          expectedState = stateEnvelopeWith (-1000) ["some-spending"]
       in playTransactions state [spending] `shouldBe` pure expectedState

    it "envelope with debt" $
      let state = stateEnvelopeWith (-1500) []
          expectedState = stateEnvelopeWith (-2500) ["some-spending"]
       in playTransactions state [spending] `shouldBe` pure expectedState

    it "spending all money in an envelope" $
      let state = stateEnvelopeWith 1000 []
          expectedState = State (Map.fromList []) $ Set.fromList [unsafeId $ unsafeNonEmpty $ unsafeText50 "some-spending"]
       in playTransactions state [spending] `shouldBe` pure expectedState

  describe "refill" $ do
    it "happy path" $
      property $ \as ->
        let amount = unArbitraryScientific as
            state = stateEnvelopeWith amount []
            expectedState = stateEnvelopeWith (amount + 1000) ["some-refill"]
         in playTransactions state [refill] `shouldBe` pure expectedState

    it "not existing envelope" $
      let state = State (Map.fromList []) Set.empty
          expectedState = stateEnvelopeWith 1000 ["some-refill"]
       in playTransactions state [refill] `shouldBe` pure expectedState

    it "envelope with debt" $
      let state = stateEnvelopeWith (-1500) []
          expectedState = stateEnvelopeWith (-500) ["some-refill"]
       in playTransactions state [refill] `shouldBe` pure expectedState

    it "fully paying off debt" $
      let state = stateEnvelopeWith (-1000) []
          expectedState = State (Map.fromList []) $ Set.fromList [unsafeId $ unsafeNonEmpty $ unsafeText50 "some-refill"]
       in playTransactions state [refill] `shouldBe` pure expectedState

stateEnvelopeWith :: Scientific -> [T.Text] -> State
stateEnvelopeWith amount applied =
  State
    ( Map.fromList
        [ ( unsafeId (unsafeNonEmpty $ unsafeText50 "fun"),
            unsafeEnvelope (unsafeNonEmpty $ unsafeText50 "fun") $
              unsafeMoney (ShorthandNumber amount) "eur"
          )
        ]
    )
    (Set.fromList $ (unsafeId . unsafeNonEmpty . unsafeText50) <$> applied)

spending :: Transaction
spending = Spending (unsafeId $ unsafeNonEmpty $ unsafeText50 "some-spending") (unsafeId $ unsafeNonEmpty $ unsafeText50 "fun") (unsafeNonEmpty $ unsafePositive $ unsafeMoney (ShorthandNumber 1000) "eur") "2023-12-14T00:00:00Z" ["tag-1"]

refill :: Transaction
refill = Refill (unsafeId $ unsafeNonEmpty $ unsafeText50 "some-refill") (unsafeId $ unsafeNonEmpty $ unsafeText50 "fun") (unsafeNonEmpty $ unsafePositive $ unsafeMoney (ShorthandNumber 1000) "eur") "2023-12-14T00:00:00Z"
