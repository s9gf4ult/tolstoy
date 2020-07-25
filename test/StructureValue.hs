module StructureValue where

import Control.Monad
import Data.Aeson as J
import Data.Scientific
import Data.String
import Data.Text as T
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
    )
    => StructureValue s
    -> SomeStructureValue

instance Show SomeStructureValue where
  show (SomeStructureValue v) = show v

instance Eq SomeStructureValue where
  (SomeStructureValue a) == (SomeStructureValue b) = cmpSval a b

showSval :: StructureValue s -> String
showSval = error "FIXME: showSval not implemented"

cmpSval :: StructureValue a -> StructureValue b -> Bool
cmpSval = error "FIXME: cmpSval not implemented"

arbText :: Gen Text
arbText = Gen.elements $ do
  let r = ["wow", "such", "random", "text"]
  a <- r
  b <- r
  c <- r
  return $ a <> b <> c

arbScientific :: Gen Scientific
arbScientific = realToFrac <$> (arbitrary :: Gen Double)

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
        s <- arbitrary :: Gen (SumValueL '[ '("some", s1), '("other", s2) ])
        return $ SomeStructureValue $ SumValue s
      prod = do
        SomeStructureValue (_ :: StructureValue s1) <- arbitrary
        SomeStructureValue (_ :: StructureValue s2) <- arbitrary
        p <- arbitrary ::
          Gen (ProductValueL '[ '("some", s1), '("other", s2) ])
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
    goSum :: SumValueL l -> Bool
    goSum = \case
      ThisValue _ s -> go s
      ThatValue rest -> goSum rest
    goProd :: ProductValueL l -> Bool
    goProd = \case
      ProductNil -> True
      ProductCons _ s rest -> go s && goProd rest

test_StructureValue :: TestTree
test_StructureValue = testGroup "Pure tests"
  [ testProperty "ToJSON/FromJSON (StructureValue s)"
    $ forAll someStructure
    $ \(s :: SomeStructureValue) -> case s of
      SomeStructureValue (v :: StructureValue s) ->
        Just v === J.decode (J.encode v)
  ]
