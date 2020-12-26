module JsonPath where

import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import           GHC.Generics (Generic)
import           Tolstoy.DSL.JsonPath.Build
import           Tolstoy.Structure

data Rec = Rec
  { string :: Text
  , sub    :: Sub
  , int    :: Int
  } deriving (Eq, Ord, Show, Generic)

instance Structural Rec

data Sub = Sub
  { string :: Text
  , int    :: Int
  } deriving (Eq, Ord, Show, Generic)

instance Structural Sub

t :: TL.Text
t = renderQuery @Rec $ root ?:
  (ctx .: prodElem @"sub" .: prodElem @"string" ==: textLit "hello")

t3 :: TL.Text
t3 = renderQuery @Rec $ root ?:
  (ctx
   .: prodElem @"sub"
   ?: (ctx .: prodElem @"int" >: numLit 10)
   .: prodElem @"string"  ==: textLit "hello")

t2 :: TL.Text
t2 = renderQuery @[Rec] $ root .: vectorAny ?:
  (ctx .: prodElem @"string" .: dotDouble ==: numLit 10)

t4 :: TL.Text
t4 = renderQuery @Rec
  $  root .: prodElem @"int"
  +: root .: prodElem @"sub" .: prodElem @"int"
