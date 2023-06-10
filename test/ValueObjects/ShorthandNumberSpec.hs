{-# LANGUAGE ImportQualifiedPost #-}

module ValueObjects.ShorthandNumberSpec (spec) where

import Data.Aeson (decode, encode)
import Data.Char qualified as Char
import Data.Text qualified as T
import Test.ArbitraryInstances (unArbitraryScientific)
import Test.Hspec
import Test.QuickCheck
import Text.Read (readMaybe)
import ValueObjects.ShorthandNumber (ShorthandNumber (..), ShorthandNumberError (..), fromText, toText)

hasSign :: String -> Bool
hasSign "" = False
hasSign str = head str `elem` ['+', '-']

endsWithKM :: String -> Bool
endsWithKM "" = False
endsWithKM str = last str `elem` ['k', 'M']

hasWhitespace :: String -> Bool
hasWhitespace = any Char.isSpace

isNumber :: String -> Bool
isNumber t = case readMaybe t :: Maybe Float of
  Just _ -> True
  Nothing -> False

spec :: Spec
spec = do
  describe "introduction - fromText" $ do
    it "returns Left on invalid number" $
      property $
        withMaxSuccess 1000 $ \str ->
          all
            ($ str)
            [ not . isNumber,
              not . hasSign,
              not . endsWithKM,
              not . hasWhitespace
            ]
            ==> let txt = T.pack str
                 in fromText txt `shouldBe` Left (InvalidNumber txt)

    describe "signs" $ do
      it "-13k" $ fromText "-13k" `shouldBe` Right (ShorthandNumber (-13000))
      it "+13k" $ fromText "+13k" `shouldBe` Right (ShorthandNumber 13000)

    describe "0" $ do
      it "0k" $ fromText "0k" `shouldBe` Right (ShorthandNumber 0)
      it "0M" $ fromText "0M" `shouldBe` Right (ShorthandNumber 0)
      it "-0k" $ fromText "-0k" `shouldBe` Right (ShorthandNumber 0)
      it "-0M" $ fromText "-0M" `shouldBe` Right (ShorthandNumber 0)
      it "+0k" $ fromText "+0k" `shouldBe` Right (ShorthandNumber 0)
      it "+0M" $ fromText "+0M" `shouldBe` Right (ShorthandNumber 0)

    describe "whitespaces" $ do
      it "whitespace before" $ fromText " 13k" `shouldBe` Right (ShorthandNumber 13000)
      it "whitespace after" $ fromText "-13k  " `shouldBe` Right (ShorthandNumber (-13000))
      it "just whitespace" $ fromText " " `shouldBe` Left (InvalidNumber " ")

    describe "happy path" $ do
      it "full number k" $ fromText "13k" `shouldBe` Right (ShorthandNumber 13000)
      it "full number M" $ fromText "1M" `shouldBe` Right (ShorthandNumber 1000000)
      it "frac number k" $ fromText "13.4k" `shouldBe` Right (ShorthandNumber 13400)
      it "frac number M" $ fromText "1.23M" `shouldBe` Right (ShorthandNumber 1230000)

  describe "elimination - toText" $ do
    it "full number k" $ toText (ShorthandNumber 13000) `shouldBe` "13k"
    it "full number M" $ toText (ShorthandNumber 1000000) `shouldBe` "1M"
    it "frac number k" $ toText (ShorthandNumber 13450) `shouldBe` "13.45k"
    it "frac number M" $ toText (ShorthandNumber 1340000) `shouldBe` "1.34M"

    describe "rounding" $ do
      it "rounding up to 2 decimal places" $ toText (ShorthandNumber 1337000) `shouldBe` "1.34M"
      it "should not display decimals if it's '00'" $ toText (ShorthandNumber 13000000.5) `shouldBe` "13M"

  describe "introduction -> elimination" $ do
    it "ShorthandNumber -> unShorthandNumber" $
      property $ \as ->
        let scinum = unArbitraryScientific as
         in unShorthandNumber (ShorthandNumber scinum) `shouldBe` scinum

    it "toText -> fromText" $
      property $
        \shortnum -> fromText (toText shortnum) `shouldBe` pure shortnum

    it "toJSON -> fromJSON" $
      property $
        \shortnum -> (decode (encode shortnum) :: Maybe ShorthandNumber) `shouldBe` pure shortnum
