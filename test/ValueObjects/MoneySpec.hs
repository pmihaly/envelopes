module ValueObjects.MoneySpec (spec) where

import Data.Aeson (decode, encode)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck
import ValueObjects.Money (Money, fromText, mkMoney, toText, unAmount, unCurrency, unsafeMoney)
import qualified ValueObjects.ShorthandNumber as SN

spec :: Spec
spec = do
  describe "introduction" $ do
    describe "mkMoney" $ do
      it "constructs a money instance" $
        property $ \shortnum str ->
          let txt = T.pack str
           in mkMoney shortnum txt `shouldBe` Right (unsafeMoney shortnum txt)

    describe "fromText" $ do
      it "constructs Money from text with currency" $
        property $ \(shortnum, str) ->
          let txt = T.pack str
           in fromText ((SN.toText shortnum) <> " " <> txt) `shouldBe` Right (unsafeMoney shortnum txt)

    describe "parseJSON" $ do
      it "constructs a money instance" $
        (decode "\"12.3k eur\"" :: Maybe Money) `shouldBe` Just (unsafeMoney 12300 "eur")

  describe "elimination" $ do
    it "can be elminated to ShorthandNumber using unAmount" $
      property $ \(shortnum, str) ->
        let txt = T.pack str
         in unAmount (unsafeMoney shortnum txt) `shouldBe` shortnum

    it "can be elminated to Text using unCurrency" $
      property $ \(shortnum, str) ->
        let txt = T.pack str
         in unCurrency (unsafeMoney shortnum txt) `shouldBe` txt

    describe "toJSON" $ do
      it "destructs a money instance" $ encode (unsafeMoney 0 "eur") `shouldBe` "\"0 eur\""

  describe "elimination -> introduction" $ do
    it "toText -> fromText" $
      property $
        \money -> fromText (toText money) `shouldBe` pure money

    it "toJSON -> fromJSON" $
      property $
        \money -> (decode (encode money) :: Maybe Money) `shouldBe` pure money

  describe "Num" $ do
    it "money + money" $
      property $
        \money1 money2 -> unAmount (money1 + money2) `shouldBe` unAmount (money1) + unAmount (money2)

    it "money - money" $
      property $
        \money1 money2 -> unAmount (money1 - money2) `shouldBe` unAmount (money1) - unAmount (money2)

  describe "Monoid" $ do
    it "mempty" $ (mempty :: Money) `shouldBe` unsafeMoney 0 ""
    it "mempty <> money" $ property $ \(money :: Money) -> mempty <> money `shouldBe` money
