{-# LANGUAGE DerivingVia #-}

module Data.Wrapper.Positive (Positive, unsafePositive, PositiveError (..), mkPositive, unPositive) where

import Control.Category ((>>>))
import Data.Aeson (FromJSON (..), ToJSON (..), withScientific)
import Data.Scientific (toBoundedInteger)
import Test.QuickCheck (Arbitrary (..), suchThat)

newtype Positive a = Positive {unPositive :: a}
  deriving (Eq, Num, ToJSON)
  deriving (Show) via a

instance (Num a, Eq a, FromJSON a, Bounded a, Integral a) => FromJSON (Positive a) where
  parseJSON =
    withScientific "Positive" $
      toBoundedInteger
        >>> maybe (Left IllegalFloat) Right
        >>> (>>= mkPositive)
        >>> either (show >>> fail) pure

instance (Num a, Eq a, Arbitrary a) => Arbitrary (Positive a) where
  arbitrary = unsafePositive <$> arbitrary `suchThat` (signum >>> (/= -1))

unsafePositive :: a -> Positive a
unsafePositive = Positive

data PositiveError
  = NegativeNotAllowed
  | IllegalFloat
  deriving (Show, Eq)

mkPositive :: (Num a, Eq a) => a -> Either PositiveError (Positive a)
mkPositive x
  | signum x == 1 = Right $ unsafePositive x
  | signum x == 0 = Right $ unsafePositive x
  | otherwise = Left NegativeNotAllowed
