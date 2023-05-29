{-# LANGUAGE TemplateHaskell #-}

module Application.State (State (State), envelopes, appliedTransactions) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.!=), (.:?), (.=))
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Entities.Envelope (Envelope, getNameAsId)
import Entities.Transaction (Transaction)
import GHC.Generics (Generic)
import Lens.Micro.Platform (makeLenses)
import Test.QuickCheck (Arbitrary (arbitrary))
import ValueObjects.Id (Id)

data State = State
  { _envelopes :: Map.HashMap (Id Envelope) Envelope,
    _appliedTransactions :: Set.HashSet (Id Transaction)
  }
  deriving (Show, Eq, Generic)

instance FromJSON State where
  parseJSON = withObject "State" $ \obj -> do
    envelopes' <- toMap <$> obj .:? "envelopes" .!= mempty
    appliedTransactions' <- Set.fromList <$> obj .:? "applied-transactions" .!= mempty
    pure $ State envelopes' appliedTransactions'

instance ToJSON State where
  toJSON (State envelopes' appliedTransactions') =
    object
      [ "envelopes" .= (fmap snd $ Map.toList envelopes'),
        "applied-transactions" .= appliedTransactions'
      ]

instance Arbitrary State where
  arbitrary = do
    piggyBalances <- toMap <$> arbitrary
    appliedTransactions <- Set.fromList <$> arbitrary
    pure $ State piggyBalances appliedTransactions

toMap :: [Envelope] -> Map.HashMap (Id Envelope) Envelope
toMap = Map.fromList <$> (fmap $ toFst getNameAsId)

toFst :: (a -> b) -> a -> (b, a)
toFst f x = (f x, x)

makeLenses ''State
