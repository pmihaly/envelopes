module Application.InputOutputFileSpec (spec) where

import Application.InputOutputFile (InputOutputFile)
import Data.Aeson (eitherDecode, encode)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "elimination -> introduction" $ do
    it "toJSON -> fromJSON" $
      property $ \ioFile ->
        (eitherDecode (encode ioFile) :: Either String InputOutputFile) `shouldBe` pure ioFile
