module ValueObjects.Money (Money, unAmount, unCurrency, unsafeMoney, MoneyError, mkMoney, fromText, toText) where

import Control.Arrow (left, second)
import Control.Category ((>>>))
import Data.Aeson (FromJSON (..), ToJSON (..), parseJSON, withText)
import qualified Data.Text as T
import Test.QuickCheck (Arbitrary (arbitrary))
import ValueObjects.ShorthandNumber (ShorthandNumber, ShorthandNumberError)
import qualified ValueObjects.ShorthandNumber as SN

data Money = Money {unAmount :: ShorthandNumber, unCurrency :: T.Text}
  deriving (Eq)

instance Show Money where
  show = toText >>> show

instance FromJSON Money where
  parseJSON = withText "Money" $ fromText >>> either (show >>> fail) pure

instance ToJSON Money where
  toJSON = toText >>> toJSON

instance Arbitrary Money where
  arbitrary = do
    amount <- arbitrary
    currency <- T.pack <$> arbitrary
    pure $ unsafeMoney amount currency

instance Num Money where
  (Money amount1 _) + (Money amount2 currency2) =
    Money (amount1 + amount2) currency2

  (Money amount1 _) - (Money amount2 currency2) =
    Money (amount1 - amount2) currency2

  (Money amount1 _) * (Money amount2 currency2) =
    Money (amount1 * amount2) currency2

  abs (Money amount currency) = Money (abs amount) currency
  signum (Money amount currency) = Money (signum amount) currency
  fromInteger amount = Money (fromInteger amount) ""

instance Semigroup Money where
  (Money amount1 _) <> (Money amount2 currency2) =
    Money (amount1 + amount2) currency2

instance Monoid Money where
  mempty = unsafeMoney 0 ""

unsafeMoney :: ShorthandNumber -> T.Text -> Money
unsafeMoney = Money

data MoneyError = NumberError ShorthandNumberError deriving (Eq, Show)

mkMoney :: ShorthandNumber -> T.Text -> Either MoneyError Money
mkMoney amount curr = pure $ unsafeMoney amount curr

fromText :: T.Text -> Either MoneyError Money
fromText txt = do
  let (amount, currency) = second (T.drop 1) $ T.breakOn " " txt
  sn <- left NumberError $ SN.fromText amount
  pure $ unsafeMoney sn currency

toText :: Money -> T.Text
toText (Money am curr) = SN.toText am <> " " <> curr
