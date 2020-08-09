module ObviousTests.Main where

import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Arbitrary.Generic
import Tolstoy.Structure

data Prod1 = Prod1
  { a :: String
  , b :: Int
  , c :: Maybe Int
  } deriving (Eq, Ord, Show, Generic)
instance Structural Prod1
instance Arbitrary Prod1 where
  arbitrary = genericArbitrary
  shrink = genericShrink

data SumA = A Int | B String
  deriving (Eq, Ord, Show, Generic)
instance Structural SumA
instance Arbitrary SumA where
  arbitrary = genericArbitrary
  shrink = genericShrink
