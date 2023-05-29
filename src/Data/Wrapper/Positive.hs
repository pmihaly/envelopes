{-# LANGUAGE DerivingVia #-}

module Data.Wrapper.Positive (Positive, unsafePositive, PositiveError (..), mkPositive, unPositive) where

import Control.Category ((>>>))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Either (isRight)
import Test.QuickCheck (Arbitrary (..), suchThat)

newtype Positive a = Positive {unPositive :: a}
  deriving (Eq, Num, ToJSON)
  deriving (Show, Semigroup, Monoid) via a

instance (Num a, Eq a, Show a, FromJSON a) => FromJSON (Positive a) where
  parseJSON value = do
    parsedValue <- parseJSON value
    case mkPositive parsedValue of
      Right positive -> pure positive
      Left e -> fail $ show e

instance (Num a, Eq a, Arbitrary a) => Arbitrary (Positive a) where
  arbitrary = unsafePositive <$> arbitrary `suchThat` (mkPositive >>> isRight)

unsafePositive :: a -> Positive a
unsafePositive = Positive

data PositiveError
  = NegativeNotAllowed
  | IllegalFloat
  deriving (Show, Eq)

mkPositive :: (Num a, Eq a) => a -> Either PositiveError (Positive a)
mkPositive x
  | abs x == x = Right $ unsafePositive x
  | otherwise = Left NegativeNotAllowed
