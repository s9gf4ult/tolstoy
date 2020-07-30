module StructureRep.Alter where

import Data.Text as T
import GHC.Generics (Generic)
import Tolstoy.Structure

data Struct1
  = Struct1Left LeftStruct1
  | Struct1Right RightStruct1
  deriving (Eq, Ord, Show, Generic)

instance Structural Struct1

data LeftStruct1 = LeftStruct1
  { this :: Int
  , that :: String
  } deriving (Eq, Ord, Show, Generic)

instance Structural LeftStruct1

data RightStruct1 = RightStruct1
  { this :: Int
  } deriving (Eq, Ord, Show, Generic)

instance Structural RightStruct1

data Struct2 = Struct2
  { field1 :: Int
  , field2 :: Text
  } deriving (Eq, Ord, Show, Generic)

instance Structural Struct2

data Struct3
  = One Int
  | Two String
  deriving (Eq, Ord, Show, Generic)

instance Structural Struct3

data Complex
  = Complex ComplexCase
  | OtherEmpty
  | Simple Int
  | Empty
  deriving (Eq, Ord, Show, Generic)

instance Structural Complex

data ComplexCase = ComplexCase
  { that :: String
  , this :: Int
  } deriving (Eq, Ord, Show, Generic)

instance Structural ComplexCase

data Broken1 = Broken1
  { this :: String
  , that :: Int
  , oups :: Int
  } deriving (Eq, Ord, Show, Generic)

instance Structural Broken1

data Broken2 = Broken2 | OtherBroken2
  deriving (Eq, Ord, Show, Generic)

instance Structural Broken2

data Broken3 = Broken3
  { this    :: Text
  , renamed :: Int
  } deriving (Eq, Ord, Show, Generic)

instance Structural Broken3

data Broken4 = Broken4
  { this  :: Text
  , field :: Text
  } deriving (Eq, Ord, Show, Generic)

instance Structural Broken4

data Broken5 = Broken5 | RenamedBroken5
  deriving (Eq, Ord, Show, Generic)

instance Structural Broken5

data Broken6 = Broken6 Int | OtherBroken6
  deriving (Eq, Ord, Show, Generic)

instance Structural Broken6
