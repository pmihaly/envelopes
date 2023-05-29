module Entities.Transaction (Transaction (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, parseJSON, withObject, (.:), (.=))
import Data.Aeson.Key (fromString, toString)
import Data.Wrapper.NonEmpty (NonEmpty)
import Data.Wrapper.Positive (Positive)
import Entities.Envelope (Envelope)
import Test.QuickCheck (Arbitrary (..), Gen, oneof)
import ValueObjects.Id (Id)
import ValueObjects.Money (Money)
import ValueObjects.Text50 (Text50)

type Date = Text50

type Tag = Text50

data Transaction
  = Spending (Id Transaction) (Id Envelope) (NonEmpty (Positive Money)) Date [Tag]
  | Refill (Id Transaction) (Id Envelope) (NonEmpty (Positive Money)) Date
  deriving (Eq, Show)

instance FromJSON Transaction where
  parseJSON = withObject "Transaction" $ \obj -> do
    (eventType :: String) <- obj .: fromString "type"
    case eventType of
      "spending" -> do
        id' <- obj .: "id"
        envelope' <- obj .: "envelope"
        amount' <- obj .: "amount"
        date' <- obj .: "date"
        tags' <- obj .: "tags"
        pure $ Spending id' envelope' amount' date' tags'
      "refill" -> do
        id' <- obj .: "id"
        envelope' <- obj .: "envelope"
        amount' <- obj .: "amount"
        date' <- obj .: "date"
        pure $ Refill id' envelope' amount' date'
      _ -> fail ("Unknown type of event" <> eventType)

instance ToJSON Transaction where
  toJSON (Spending id' envelope' amount' date' tags') =
    object ["id" .= id', "type" .= toString "spending", "envelope" .= envelope', "amount" .= amount', "date" .= date', "tags" .= tags']
  toJSON (Refill id' envelope' amount' date') =
    object ["id" .= id', "type" .= toString "refill", "envelope" .= envelope', "amount" .= amount', "date" .= date']

instance Arbitrary Transaction where
  arbitrary = oneof [spendingGenerator, refillGenerator]

spendingGenerator :: Gen Transaction
spendingGenerator = Spending <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

refillGenerator :: Gen Transaction
refillGenerator = Refill <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
