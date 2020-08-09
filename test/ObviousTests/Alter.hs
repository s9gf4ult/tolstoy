module ObviousTests.Alter where

import Control.DeepSeq
import Data.Vector as V
import GHC.Generics (Generic)
import Tolstoy.Structure

data SumB = B String | A Int
  deriving (Eq, Ord, Show, Generic)
instance Structural SumB where
instance NFData SumB

data Prod2 = Prod2
  { a :: Maybe String
  , b :: Vector Int
  , c :: Maybe Int
  } deriving (Eq, Ord, Show, Generic)
instance Structural Prod2
instance NFData Prod2

data Prod3 = Prod3
  { b :: Maybe Int
  , c :: Vector (Maybe Int)
  } deriving (Eq, Ord, Show, Generic)
instance Structural Prod3
instance NFData Prod3

data Prod4 = Prod4
  { c :: Maybe (Vector Int)
  } deriving (Eq, Ord, Show, Generic)
instance Structural Prod4
instance NFData Prod4

data Prod5 = Prod5
  { a :: String
  , b :: Int
  , c :: Maybe Int
  , d :: Maybe String
  } deriving (Eq, Ord, Show, Generic)
instance Structural Prod5
instance NFData Prod5

data Wrapped = Wrapped Single
  deriving (Eq, Ord, Show, Generic)
instance Structural Wrapped

data Single = Single
  { single :: Int
  } deriving (Eq, Ord, Show, Generic)
instance Structural Single
