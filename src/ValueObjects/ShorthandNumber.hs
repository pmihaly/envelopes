{-# LANGUAGE ImportQualifiedPost #-}

module ValueObjects.ShorthandNumber (ShorthandNumber (..), ShorthandNumberError (..), fromText, toText) where

import Control.Category ((>>>))
import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.Char (isDigit)
import Data.Scientific (Scientific, isInteger, toRealFloat)
import Data.Text qualified as T
import Numeric (showFFloat)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, elements)
import Text.Read (readMaybe)

newtype ShorthandNumber = ShorthandNumber {unShorthandNumber :: Scientific}
  deriving newtype (Eq, Num)

instance Show ShorthandNumber where
  show = toText >>> show

instance FromJSON ShorthandNumber where
  parseJSON =
    withText "ShorthandNumber" $
      fromText
        >>> either (show >>> fail) pure

instance ToJSON ShorthandNumber where
  toJSON = toText >>> toJSON

instance Arbitrary ShorthandNumber where
  arbitrary = do
    coefficient <- arbitrary :: Gen Int
    scale <- elements ([1e3, 1e6] :: [Double])
    let exponent' = 2 :: Int
        value = fromIntegral coefficient * scale * 10 ^^ exponent'
    pure (ShorthandNumber (fromRational (toRational value)))

data ShorthandNumberError = InvalidNumber T.Text deriving (Eq, Show)

fromText :: T.Text -> Either ShorthandNumberError ShorthandNumber
fromText fulltext
  | T.null trimmedText = Left $ InvalidNumber fulltext
  | otherwise = do
      scale <- getScalingFactor suffix
      parsed <- parseScientific numeric
      pure $ ShorthandNumber $ parsed * scale
  where
    trimmedText = T.strip fulltext
    suffix = T.last trimmedText
    numeric = if isDigit suffix then trimmedText else T.init trimmedText

    getScalingFactor 'k' = Right 1e3
    getScalingFactor 'M' = Right 1e6
    getScalingFactor suffix'
      | isDigit suffix' = Right 1
      | otherwise = Left $ InvalidNumber fulltext

    parseScientific =
      maybe (Left $ InvalidNumber fulltext) Right . readMaybe . T.unpack

toText :: ShorthandNumber -> T.Text
toText (ShorthandNumber value)
  | magnitude >= 1e6 = formatNumber (value / 1e6) <> "M"
  | magnitude >= 1e3 = formatNumber (value / 1e3) <> "k"
  | otherwise = formatNumber value
  where
    magnitude = abs value
    formatNumber num
      | isInteger num || num - fromIntegral (floor num :: Integer) < 0.01 = T.pack (show (round num :: Integer))
      | otherwise = T.pack (showFFloat (Just 2) (toRealFloat num :: Float) "")
