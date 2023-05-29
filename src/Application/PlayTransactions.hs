{-# LANGUAGE EmptyDataDeriving #-}

module Application.PlayTransactions (playTransactions) where

import Application.State (State (..), appliedTransactions, envelopes)
import Control.Arrow (left)
import Control.Monad (foldM)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Entities.Envelope (Envelope, EnvelopeError, toNothingIfEmpty)
import qualified Entities.Envelope as E
import Entities.Transaction (Transaction (..), getTransactionId)
import Lens.Micro
import ValueObjects.Id (Id, unId)

newtype ApplicationError = TransactionError EnvelopeTransactionError deriving (Eq, Show)

playTransactions :: State -> [Transaction] -> Either ApplicationError State
playTransactions =
  foldM
    ( \state tx -> ifNotAlreadyApplied state tx $ do
        newState <- left TransactionError $ onTransaction state tx
        pure $ newState & appliedTransactions %~ Set.insert (getTransactionId tx)
    )

ifNotAlreadyApplied :: State -> Transaction -> Either ApplicationError State -> Either ApplicationError State
ifNotAlreadyApplied state tx newState =
  if Set.member (getTransactionId tx) (state ^. appliedTransactions)
    then pure state
    else newState

data EnvelopeTransactionError = EnvelopeError E.EnvelopeError deriving (Eq, Show)

onTransaction :: State -> Transaction -> Either EnvelopeTransactionError State
onTransaction state (Spending _ envelopeId amount _ _) = setEnvelopeMoney (E.withdraw amount) state envelopeId
onTransaction state (Refill _ envelopeId amount _) = setEnvelopeMoney (E.deposit amount) state envelopeId

setEnvelopeMoney ::
  (Envelope -> Either EnvelopeError Envelope) ->
  State ->
  Id Envelope ->
  Either EnvelopeTransactionError State
setEnvelopeMoney operate state envelopeId = do
  defaultEnvelope <- left EnvelopeError $ E.mkEnvelope (unId envelopeId) mempty
  let oldEnvelope = Map.findWithDefault defaultEnvelope envelopeId (state ^. envelopes)
  newEnvelope <- left EnvelopeError $ operate oldEnvelope
  pure $ envelopes %~ (Map.alter (const $ toNothingIfEmpty newEnvelope) envelopeId) $ state
