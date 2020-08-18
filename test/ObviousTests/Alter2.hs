module ObviousTests.Alter2 where

import Control.DeepSeq
import Data.Text as T
import Data.Vector as V
import GHC.Generics (Generic)
import Tolstoy.Structure

data SumC = A (Maybe Int) | B (Vector Text) | C (Maybe Int)
  deriving (Eq, Ord, Show, Generic)
instance Structural SumC
instance NFData SumC
