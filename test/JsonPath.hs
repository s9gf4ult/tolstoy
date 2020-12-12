module JsonPath where

import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import           GHC.Generics (Generic)
import           Tolstoy.DSL.JsonPath.Build
import           Tolstoy.Structure

data Rec = Rec
  { string :: Text
  , sub    :: Sub
  } deriving (Eq, Ord, Show, Generic)

instance Structural Rec

data Sub = Sub
  { string :: Text
  , int    :: Int
  } deriving (Eq, Ord, Show, Generic)

instance Structural Sub

t :: TL.Text
t = renderQuery @Rec $ root ?:
  (query (ctx .: prodElem @"sub" .: prodElem @"string") ==: textLit "hello")
