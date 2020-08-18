module StructureRep.Main where

import Data.Text as T
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Arbitrary.Generic
import Tolstoy.Structure

data Struct1
  = Struct1Left
    { this :: Int
    , that :: Text }
  | Struct1Right
    { this :: Int }
  deriving (Eq, Ord, Show, Generic)

instance Structural Struct1
instance Arbitrary Struct1 where
  arbitrary = genericArbitrary
  shrink = genericShrink

data Struct2 = Struct2
  { field2 :: Text
  , field1 :: Int
  } deriving (Eq, Ord, Show, Generic)

instance Structural Struct2
instance Arbitrary Struct2 where
  arbitrary = genericArbitrary
  shrink = genericShrink

data Struct3
  = Two Text
  | One Int
  deriving (Eq, Ord, Show, Generic)

instance Structural Struct3
instance Arbitrary Struct3 where
  arbitrary = genericArbitrary
  shrink = genericShrink

data Complex
  = Complex
  { this :: Int
  , that :: Text }
  | Simple Int
  | Empty
  | OtherEmpty
  deriving (Eq, Ord, Show, Generic)

instance Structural Complex
instance Arbitrary Complex where
  arbitrary = genericArbitrary
  shrink = genericShrink

data Broken1 = Broken1
  { this :: Text
  , that :: Int
  } deriving (Eq, Ord, Show, Generic)

instance Structural Broken1

data Broken2 = Broken2
  deriving (Eq, Ord, Show, Generic)

instance Structural Broken2

data Broken3 = Broken3
  { this  :: Text
  , field :: Int
  } deriving (Eq, Ord, Show, Generic)

instance Structural Broken3

data Broken4 = Broken4
  { this  :: Text
  , field :: Int
  } deriving (Eq, Ord, Show, Generic)

instance Structural Broken4

data Broken5 = Broken5 | OtherBroken5
  deriving (Eq, Ord, Show, Generic)

instance Structural Broken5

data Broken6 = Broken6 | OtherBroken6
  deriving (Eq, Ord, Show, Generic)

instance Structural Broken6
