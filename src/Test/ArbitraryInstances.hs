{-# LANGUAGE FlexibleInstances #-}

module Test.ArbitraryInstances (ArbitraryScientific (..)) where

import Data.Scientific
import Test.QuickCheck

newtype ArbitraryScientific = ArbitraryScientific {unArbitraryScientific :: Scientific} deriving (Show, Eq)

instance Arbitrary ArbitraryScientific where
  arbitrary = do
    coefficient <- arbitrary :: Gen Integer
    exponent' <- arbitrary :: Gen Int
    return $ ArbitraryScientific (scientific coefficient exponent')
