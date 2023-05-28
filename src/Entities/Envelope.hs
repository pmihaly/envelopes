{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Entities.Envelope (Envelope, name, balance, unsafeEnvelope, EnvelopeError, mkEnvelope) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, parseJSON, withObject, (.!=), (.:), (.:?), (.=))
import Lens.Micro.Platform (makeLenses)
import Test.QuickCheck (Arbitrary (arbitrary))
import ValueObjects.Money (Money)
import ValueObjects.Text50 (Text50)

data Envelope = Envelope {_name :: Text50, _balance :: Money}
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
  (Envelope "" balance1) <> (Envelope name2 balance2) =
    Envelope name2 (balance1 <> balance2)
  (Envelope name1 balance1) <> (Envelope "" balance2) =
    Envelope name1 (balance1 <> balance2)
  (Envelope name1 balance1) <> (Envelope name2 balance2) =
    Envelope (name1 <> " and " <> name2) (balance1 <> balance2)

instance Monoid Envelope where
  mempty = Envelope "" mempty

unsafeEnvelope :: Text50 -> Money -> Envelope
unsafeEnvelope = Envelope

data EnvelopeError deriving (Eq, Show)

mkEnvelope :: Text50 -> Money -> Either EnvelopeError Envelope
mkEnvelope amount curr = pure $ unsafeEnvelope amount curr
