{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Entities.Envelope (Envelope, name, balance, unsafeEnvelope, EnvelopeError, mkEnvelope, getNameAsId, withdraw, deposit, toNothingIfEmpty) where

import Control.Category ((>>>))
import Data.Aeson (FromJSON (..), ToJSON (..), object, parseJSON, withObject, (.!=), (.:), (.:?), (.=))
import Data.Wrapper.NonEmpty (NonEmpty, unNonEmpty, unsafeNonEmpty)
import Data.Wrapper.Positive (Positive, unPositive)
import Lens.Micro
import Lens.Micro.Platform (makeLenses)
import Test.QuickCheck (Arbitrary (arbitrary))
import ValueObjects.Id (Id, unsafeId)
import ValueObjects.Money (Money, unAmount)
import ValueObjects.Text50 (Text50)

data Envelope = Envelope {_name :: NonEmpty Text50, _balance :: Money}
  deriving (Eq, Show)

makeLenses ''Envelope

instance FromJSON Envelope where
  parseJSON = withObject "Envelope" $ \obj -> do
    name' <- obj .: "name"
    balance' <- obj .:? "balance" .!= mempty
    pure $ Envelope name' balance'

instance ToJSON Envelope where
  toJSON (Envelope name' balance') =
    object
      [ "name" .= name',
        "balance" .= balance'
      ]

instance Arbitrary Envelope where
  arbitrary = do
    name' <- arbitrary
    money' <- arbitrary
    pure $ unsafeEnvelope name' money'

instance Semigroup Envelope where
  (Envelope name1 balance1) <> (Envelope name2 balance2) =
    Envelope (name1 <> (unsafeNonEmpty " and ") <> name2) (balance1 <> balance2)

instance Monoid Envelope where
  mempty = Envelope (unsafeNonEmpty "an unnamed envelope") mempty

unsafeEnvelope :: NonEmpty Text50 -> Money -> Envelope
unsafeEnvelope = Envelope

data EnvelopeError deriving (Eq, Show)

mkEnvelope :: NonEmpty Text50 -> Money -> Either EnvelopeError Envelope
mkEnvelope amount curr = pure $ unsafeEnvelope amount curr

getNameAsId :: Envelope -> Id Envelope
getNameAsId = (^. name) >>> unsafeId

withdraw :: NonEmpty (Positive Money) -> Envelope -> Either EnvelopeError Envelope
withdraw amount envelope = pure $ balance %~ (flip (-) $ unPositive $ unNonEmpty amount) $ envelope

deposit :: NonEmpty (Positive Money) -> Envelope -> Either EnvelopeError Envelope
deposit amount envelope = pure $ balance %~ (flip (+) $ unPositive $ unNonEmpty amount) $ envelope

toNothingIfEmpty :: Envelope -> Maybe Envelope
toNothingIfEmpty en = if ((== 0) $ unAmount $ en ^. balance) then Nothing else Just en
