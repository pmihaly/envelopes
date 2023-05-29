{-# LANGUAGE EmptyDataDeriving #-}

module Application.PlayTransactions (playTransactions) where

import Application.State (State (..), appliedTransactions)
import Control.Monad (foldM)
import qualified Data.HashSet as Set
import Entities.Transaction (Transaction, getTransactionId)
import Lens.Micro

data ApplicationError deriving (Eq, Show)

playTransactions :: State -> [Transaction] -> Either ApplicationError State
playTransactions =
  foldM
    ( \state tx -> ifNotAlreadyApplied state tx $ do
        pure $ state & appliedTransactions %~ Set.insert (getTransactionId tx)
    )

ifNotAlreadyApplied :: State -> Transaction -> Either ApplicationError State -> Either ApplicationError State
ifNotAlreadyApplied state tx newState =
  if Set.member (getTransactionId tx) (state ^. appliedTransactions)
    then pure state
    else newState
