module Application.PlayTransactionsSpec (spec) where

import Application.PlayTransactions (playTransactions)
import Application.State (appliedTransactions)
import Control.Category ((>>>))
import qualified Data.HashSet as Set
import Entities.Transaction (getTransactionId)
import Lens.Micro.Platform
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  it "should insert transactionId to appliedTransactions" $
    property $
      \state transaction ->
        (not $ (getTransactionId transaction) `elem` state ^. appliedTransactions)
          ==> (\state' -> getTransactionId transaction `elem` (state' ^. appliedTransactions)) <$> playTransactions state [transaction]
          `shouldBe` pure True

  describe "idempotence" $ do
    it "should not apply the same transaction twice" $
      property $
        withMaxSuccess 1000 $ \state transaction ->
          playTransactions state [transaction] `shouldBe` playTransactions state [transaction, transaction]
