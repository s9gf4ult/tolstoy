module UsersExample.Data.Shared where

import Data.String
import Data.Text as T
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen as Arb
import Tolstoy.DB
import Tolstoy.Migration
import Tolstoy.Structure
import Tolstoy.Types

newtype Name = Name
  { unName :: Text
  } deriving (Eq, Ord, Show, Generic, Structural, IsString)

instance Arbitrary Name where
  arbitrary = Arb.elements ["lupa", "pupa", "pepa"]

newtype Email = Email
  { unEmail :: Text
  } deriving (Eq, Ord, Show, Generic, Structural, IsString)

instance Arbitrary Email where
  arbitrary = Arb.elements $ do
    user <- ["user", "buzer", "muzer"]
    host <- ["@example.com", "@super.mail", "@google.com"]
    return $ Email $ user <> host
