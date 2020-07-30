module StructureRep.Main where

import Data.Text as T
import GHC.Generics (Generic)
import Tolstoy.Structure

data Struct1
  = Struct1Left
    { this :: Int
    , that :: String }
  | Struct1Right
    { this :: Int }
  deriving (Eq, Ord, Show, Generic)

instance Structural Struct1

data Struct2 = Struct2
  { field2 :: Text
  , field1 :: Int
  } deriving (Eq, Ord, Show, Generic)

instance Structural Struct2

data Struct3
  = Two String
  | One Int
  deriving (Eq, Ord, Show, Generic)

instance Structural Struct3

data Complex
  = Complex
  { this :: Int
  , that :: String }
  | Simple Int
  | Empty
  | OtherEmpty
  deriving (Eq, Ord, Show, Generic)

instance Structural Complex

data Broken1 = Broken1
  { this :: String
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
