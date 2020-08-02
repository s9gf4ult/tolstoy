module TestMigrationsExample where

import Control.DeepSeq
import Control.Lens
import Control.Monad
import Data.Aeson as J
import Data.Generics.Product
import Data.Int
import Data.Maybe
import Data.Scientific
import Data.String
import Data.Text as T
import Data.Typeable
import Data.Vector as V
import GHC.Generics (Generic)
import Test.Hspec.Expectations
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen as Gen
import Test.QuickCheck.Instances ()
import Test.Tasty as Test
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tolstoy.Migrations
import Tolstoy.Migration
import Tolstoy.Structure

data Rec1 = Rec1
  { a :: Int32
  , b :: String
  } deriving (Eq, Ord, Show, Generic)

instance Structural Rec1
instance Arbitrary Rec1 where
  arbitrary = genericArbitrary
  shrink = genericShrink

data Rec2 = Rec2
  { a :: Int64
  , b :: String
  } deriving (Eq, Ord, Show, Generic)

instance Structural Rec2
instance Arbitrary Rec2 where
  arbitrary = genericArbitrary
  shrink = genericShrink
instance NFData Rec2

rec1to2 :: Rec1 -> Rec2
rec1to2 r = Rec2
  { a = fromIntegral $ r ^. field @"a"
  , b = r ^. field @"b"
  }

migrations :: Migrations 0 '[Rec1, Rec2]
migrations
  = Migrate Proxy rec1to2
  $ LastVersion Proxy Proxy

customChecks :: Checks AnyTypes 0 '[Rec1, Rec2]
customChecks = Check Proxy rec1Check $ LastCheck
  where
    rec1Check f = forAll arbitrary $ \rec1 ->
      toInteger (rec1 ^. field @"a") === toInteger (f rec1 ^. field @"a")

test_Migrations :: TestTree
test_Migrations = testGroup "Migrations"
  [ testGroup "allTotal" $
    uncurry testProperty <$> genMigrationsTests migrations allTotal
  , testGroup "custom checks" $
    uncurry testProperty <$> genMigrationsTests migrations customChecks
  ]
