module ObviousTests.Alter2 where

import Control.DeepSeq
import Data.Vector as V
import GHC.Generics (Generic)
import Tolstoy.Structure

data SumC = A (Maybe Int) | B (Vector String) | C (Maybe Int)
  deriving (Eq, Ord, Show, Generic)
instance Structural SumC
instance NFData SumC
