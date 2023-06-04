{-# LANGUAGE TemplateHaskell #-}

module Application.InputOutputFile (InputOutputFile (InputOutputFile), state, transactions) where

import Application.State (State)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.!=), (.:?), (.=))
import Data.Aeson.Types (Object)
import Entities.Transaction (Transaction)
import Lens.Micro.Platform (makeLenses)
import Test.QuickCheck (Arbitrary (arbitrary))

data InputOutputFile = InputOutputFile
  { _state :: State,
    _transactions :: [Transaction]
  }
  deriving (Show, Eq)

instance FromJSON InputOutputFile where
  parseJSON = withObject "InputOutputFile" $ \obj -> do
    state' <- parseJSON (Object obj)
    transactions' <- obj .:? "transactions" .!= []
    pure $ InputOutputFile state' transactions'

instance ToJSON InputOutputFile where
  toJSON (InputOutputFile state' transactions') =
    mergeObjects [toJSON state', object ["transactions" .= transactions']]

mergeObjects :: [Value] -> Value
mergeObjects = Object . mconcat . map getObject

getObject :: Value -> Object
getObject (Object obj) = obj
getObject _ = mempty

instance Arbitrary InputOutputFile where
  arbitrary = InputOutputFile <$> arbitrary <*> arbitrary

makeLenses ''InputOutputFile
