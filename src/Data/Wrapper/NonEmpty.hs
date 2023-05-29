{-# LANGUAGE DerivingVia #-}

module Data.Wrapper.NonEmpty (NonEmpty, unsafeNonEmpty, NonEmptyError (..), mkNonEmpty, unNonEmpty) where

import Control.Category ((>>>))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Either (isRight)
import Data.Hashable (Hashable)
import Test.QuickCheck (Arbitrary (..), suchThat)

newtype NonEmpty a = NonEmpty {unNonEmpty :: a}
  deriving (Eq, Semigroup, Monoid)
  deriving (Show, ToJSON, Hashable) via a

instance (Monoid a, Eq a, FromJSON a) => FromJSON (NonEmpty a) where
  parseJSON value = do
    parsedValue <- parseJSON value
    case mkNonEmpty parsedValue of
      Right nonempty -> pure nonempty
      Left e -> fail $ show e

instance (Monoid a, Eq a, Arbitrary a) => Arbitrary (NonEmpty a) where
  arbitrary = unsafeNonEmpty <$> arbitrary `suchThat` (mkNonEmpty >>> isRight)

unsafeNonEmpty :: a -> NonEmpty a
unsafeNonEmpty = NonEmpty

data NonEmptyError
  = EmptyNotAllowed
  deriving (Show, Eq)

mkNonEmpty :: (Monoid a, Eq a) => a -> Either NonEmptyError (NonEmpty a)
mkNonEmpty x
  | x == mempty = Left EmptyNotAllowed
  | otherwise = Right $ unsafeNonEmpty x
