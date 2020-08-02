module StructureValue where

import Control.Monad
import Data.Aeson as J
import Data.Scientific
import Data.String
import Data.Text as T
import Data.Typeable
import Data.Vector as V
import Prelude as P
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen as Gen
import Test.QuickCheck.Instances ()
import Test.Tasty as Test
import Test.Tasty.QuickCheck
import Tolstoy.Structure

data SomeStructureValue where
  SomeStructureValue :: forall s.
    ( FromJSON (StructureValue s)
    , Eq (StructureValue s)
    , Arbitrary (StructureValue s)
    , Typeable (StructureValue s)
    , Typeable s )
    => StructureValue s
    -> SomeStructureValue

instance Show SomeStructureValue where
  show (SomeStructureValue v) = show v

instance Eq SomeStructureValue where
  (SomeStructureValue a) == (SomeStructureValue b) = case cast a of
    Just a' -> a' == b
    Nothing -> False

instance Arbitrary SomeStructureValue where
  arbitrary = oneof allCases
    where
      allCases =
        [ SomeStructureValue . StringValue <$> arbitrary
        , SomeStructureValue . NumberValue <$> arbitrary
        , SomeStructureValue . BoolValue <$> arbitrary
        , opt
        , vec
        , sumSt
        , prod
        ]
      opt = do
        SomeStructureValue s <- arbitrary
        sub <- oneof [ pure $ Just s, pure Nothing ]
        return $ SomeStructureValue $ OptionalValue sub
      vec = do
        SomeStructureValue (_ :: StructureValue s) <- arbitrary
        v <- arbitrary :: Gen (Vector (StructureValue s))
        return $ SomeStructureValue $ VectorValue v
      sumSt = do
        SomeStructureValue (_ :: StructureValue s1) <- arbitrary
        SomeStructureValue (_ :: StructureValue s2) <- arbitrary
        s <- arbitrary
          :: Gen (SumTreeValue ('Sum2 ('Sum1 "some" s1) ('Sum1 "other" s2)))
        return $ SomeStructureValue $ SumValue s
      prod = do
        SomeStructureValue (_ :: StructureValue s1) <- arbitrary
        SomeStructureValue (_ :: StructureValue s2) <- arbitrary
        p <- arbitrary :: Gen (ProductTreeValue
                               ('Product2
                                ('Product1 "some" s1)
                                ('Product1 "other" s2)))
        return $ SomeStructureValue $ ProductValue p

someStructure :: Gen SomeStructureValue
someStructure = suchThat arbitrary noJustNothing

noJustNothing :: SomeStructureValue -> Bool
noJustNothing (SomeStructureValue a) = go a
  where
    go :: StructureValue s -> Bool
    go = \case
      StringValue {} -> True
      NumberValue {} -> True
      BoolValue {}   -> True
      OptionalValue (Just sub) -> case sub of
        OptionalValue Nothing -> False
        _                     -> go sub
      OptionalValue Nothing -> True
      VectorValue v -> P.all go v
      SumValue s -> goSum s
      ProductValue p -> goProd p
    goSum :: SumTreeValue t -> Bool
    goSum = \case
      Sum1Value _ s -> go s
      Sum2Left l -> goSum l
      Sum2Right r -> goSum r
    goProd :: ProductTreeValue t -> Bool
    goProd = \case
      Product0Value -> True
      Product1Value _ s -> go s
      Product2Value p1 p2 -> goProd p1 && goProd p2

test_StructureValue :: TestTree
test_StructureValue = testGroup "Pure tests"
  [ testProperty "ToJSON/FromJSON (StructureValue s)"
    $ forAll someStructure
    $ \(s :: SomeStructureValue) -> case s of
      SomeStructureValue (v :: StructureValue s) ->
        Just v === J.decode (J.encode v)
  ]
