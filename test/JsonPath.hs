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
  , may    :: Maybe Text
  , maySub :: Maybe Sub
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

t5 :: TL.Text
t5 = renderQuery @Rec
  $ root ?: (ctx .: prodElem @"may" ==: nullLit)

t7 :: TL.Text
t7 = renderQuery @Rec
  $ root ?: (ctx .: prodElem @"may" ==: textLit "something")

-- t8 :: TL.Text
-- t8 = renderQuery @Rec
--   $ root ?: (ctx .: prodElem @"maySub" ==: ctx .: prodElem @"sub")

t9 :: TL.Text
t9 = renderQuery @Rec
  $ root ?: (ctx .: prodElem @"may" ==: ctx .: prodElem @"string")

t6 :: TL.Text
t6 = renderQuery @Rec
  $ root ?: (ctx .: prodElem @"maySub" ==: nullLit)
