{-# LANGUAGE ImportQualifiedPost #-}

module ValueObjects.Id (Id, unId, mkId, IdError (..), unsafeId) where

import Control.Arrow (left, right)
import Control.Category ((>>>))
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON, ToJSONKey, withText)
import Data.Hashable (Hashable)
import Data.Text qualified as T
import Data.Wrapper.NonEmpty (NonEmpty, NonEmptyError (..), mkNonEmpty)
import Test.QuickCheck (Arbitrary)
import ValueObjects.Text50 (Text50, Text50Error, mkText50)

newtype Id a = Id {unId :: NonEmpty Text50}
  deriving newtype (Show, Eq, Arbitrary, ToJSON, Hashable)

instance FromJSON (Id a) where
  parseJSON = withText "Id" $ mkId >>> either (show >>> fail) pure

instance FromJSONKey (Id a)

instance ToJSONKey (Id a)

unsafeId :: NonEmpty Text50 -> Id a
unsafeId = Id

data IdError
  = InvalidText Text50Error
  | InvalidEmpty NonEmptyError
  deriving (Show, Eq)

mkId :: T.Text -> Either IdError (Id a)
mkId =
  mkText50
    >>> left InvalidText
    >>> (>>= mkNonEmpty >>> left InvalidEmpty)
    >>> right unsafeId
