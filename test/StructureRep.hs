module StructureRep where

import Control.Monad
import Data.Aeson as J
import Data.Scientific
import Data.String
import Data.Text as T
import Data.Typeable
import Data.Vector as V
import Prelude as P
import StructureRep.Alter as Alter
import StructureRep.Main as Main
import Test.Hspec.Expectations
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen as Gen
import Test.QuickCheck.Instances ()
import Test.Tasty as Test
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Tolstoy.Structure

test_Available :: TestTree
test_Available = testGroup "Available structure changings"
  [ testCase "Split constructors" $ shouldBe
        (toJSON (structureRep :: StructureRep (StructKind Main.Struct1)))
        (toJSON (structureRep :: StructureRep (StructKind Alter.Struct1)))
  , testCase "Change product fields order" $ shouldBe
        (toJSON (structureRep :: StructureRep (StructKind Main.Struct2)))
        (toJSON (structureRep :: StructureRep (StructKind Alter.Struct2)))
  , testCase "Change sum fields order" $ shouldBe
        (toJSON (structureRep :: StructureRep (StructKind Main.Struct3)))
        (toJSON (structureRep :: StructureRep (StructKind Alter.Struct3)))
  , testCase "Complex case" $ shouldBe
        (toJSON (structureRep :: StructureRep (StructKind Main.Complex)))
        (toJSON (structureRep :: StructureRep (StructKind Alter.Complex)))
  ]

test_Broken :: TestTree
test_Broken = testGroup "Not available structure changings"
  [ testCase "Add product field" $ shouldNotBe
    (toJSON (structureRep :: StructureRep (StructKind Main.Broken1)))
    (toJSON (structureRep :: StructureRep (StructKind Alter.Broken1)))
  , testCase "Add sum field" $ shouldNotBe
    (toJSON (structureRep :: StructureRep (StructKind Main.Broken2)))
    (toJSON (structureRep :: StructureRep (StructKind Alter.Broken2)))
  , testCase "Rename product field" $ shouldNotBe
    (toJSON (structureRep :: StructureRep (StructKind Main.Broken3)))
    (toJSON (structureRep :: StructureRep (StructKind Alter.Broken3)))
  , testCase "Change product type" $ shouldNotBe
    (toJSON (structureRep :: StructureRep (StructKind Main.Broken4)))
    (toJSON (structureRep :: StructureRep (StructKind Alter.Broken4)))
  , testCase "Rename sum field" $ shouldNotBe
    (toJSON (structureRep :: StructureRep (StructKind Main.Broken5)))
    (toJSON (structureRep :: StructureRep (StructKind Alter.Broken5)))
  , testCase "Change sum type" $ shouldNotBe
    (toJSON (structureRep :: StructureRep (StructKind Main.Broken6)))
    (toJSON (structureRep :: StructureRep (StructKind Alter.Broken6)))


  ]
