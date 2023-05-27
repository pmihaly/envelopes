{-# LANGUAGE DerivingVia #-}

module Data.Wrapper.NonEmpty (NonEmpty, unsafeNonEmpty, NonEmptyError (..), mkNonEmpty, unNonEmpty) where

import Data.Aeson (FromJSON (..), Result (..), ToJSON (..), Value (..), fromJSON, withText)
import Data.Hashable (Hashable)
import Test.QuickCheck (Arbitrary (..), suchThat)

newtype NonEmpty a = NonEmpty {unNonEmpty :: a}
  deriving (Eq, Semigroup, Monoid)
  deriving (Show, ToJSON, Hashable) via a

instance (Monoid a, Eq a, FromJSON a) => FromJSON (NonEmpty a) where
  parseJSON = withText "NonEmpty" $ \txt ->
    case fromJSON (String txt) of
      Success val -> case mkNonEmpty val of
        Right nonEmpty -> pure nonEmpty
        Left e -> fail $ show e
      Error e -> fail $ show e

instance (Monoid a, Eq a, Arbitrary a) => Arbitrary (NonEmpty a) where
  arbitrary = unsafeNonEmpty <$> arbitrary `suchThat` (/= mempty)

unsafeNonEmpty :: a -> NonEmpty a
unsafeNonEmpty = NonEmpty

data NonEmptyError
  = EmptyNotAllowed
  deriving (Show, Eq)

mkNonEmpty :: (Monoid a, Eq a) => a -> Either NonEmptyError (NonEmpty a)
mkNonEmpty x
  | x == mempty = Left EmptyNotAllowed
  | otherwise = Right $ unsafeNonEmpty x
