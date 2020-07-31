{-# LANGUAGE AllowAmbiguousTypes #-}

module StructureRep where

import Control.DeepSeq
import Control.Monad
import Data.Aeson as J
import Data.Maybe
import Data.Scientific
import Data.String
import Data.Text as T
import Data.Typeable
import Data.Vector as V
import Prelude as P
import StructureRep.Alter as Alter
import StructureRep.Main as Main
import Test.Hspec.Expectations
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen as Gen
import Test.QuickCheck.Instances ()
import Test.Tasty as Test
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Tolstoy.Structure

jsonProp
  :: forall a b.
  ( Structural a
  , Structural b
  , NFData b
  , FromJSON (StructureValue (StructKind b)))
  => a
  -> Property
jsonProp a =
  let
    dec :: Maybe (StructureValue (StructKind b))
    dec = J.decode $ J.encode $ toStructValue a
  in case fromStructValue <$> dec of
    Nothing       -> error "Got Nothing"
    Just (b :: b) -> total b

avaiTree
  :: forall a b.
  ( Structural a
  , Structural b
  , NFData b
  , FromJSON (StructureValue (StructKind b))
  , KnownStructure (StructKind a)
  , KnownStructure (StructKind b)
  , Arbitrary a
  , Show a )
  => TestName
  -> TestTree
avaiTree n = testGroup n
  [ testCase "Rep compatible" $ shouldBe
    (toJSON (structureRep :: StructureRep (StructKind a)))
    (toJSON (structureRep :: StructureRep (StructKind b)))
  , testProperty "Value FromJSON/ToJSON" $ jsonProp @a @b ]

test_Available :: TestTree
test_Available = testGroup "Available structure changings"
  [ avaiTree @Main.Struct1 @Alter.Struct1 "Split constructors"
  , avaiTree @Main.Struct2 @Alter.Struct2 "Change product fields order"
  , avaiTree @Main.Struct3 @Alter.Struct3  "Change sum fields order"
  , avaiTree @Main.Complex @Alter.Complex "Complex case"
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
