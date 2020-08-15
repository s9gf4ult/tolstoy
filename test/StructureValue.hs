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

resizeStruct :: Gen a -> Gen a
resizeStruct = scale (\a -> max 0 $ a - 10)

instance Arbitrary SomeStructureValue where
  arbitrary = sized $ \case
    0 -> prod0
    _ -> oneof allCases
    where
      allCases =
        [ SomeStructureValue . StringValue <$> arbitrary
        , SomeStructureValue . NumberValue <$> arbitrary
        , SomeStructureValue . BoolValue <$> arbitrary
        , opt
        , vec
        , sum2
        , sum1
        , prod2
        , prod1
        , prod0
        ]
      opt = do
        SomeStructureValue (s :: StructureValue s) <- resizeStruct arbitrary
        sub <- sized $ \case
          0 -> pure Nothing
          _ -> oneof [ pure $ Just s, pure Nothing ]
        return $ SomeStructureValue $ OptionalValue sub
      vec = do
        SomeStructureValue (_ :: StructureValue s) <- resizeStruct arbitrary
        v <- resizeStruct arbitrary :: Gen (Vector (StructureValue s))
        return $ SomeStructureValue $ VectorValue v
      sum2 = do
        SomeStructureValue (s1 :: StructureValue s1) <- resizeStruct arbitrary
        SomeStructureValue (s2 :: StructureValue s2) <- resizeStruct arbitrary
        let
          -- a :: Gen (SumTreeValue ('Sum2 ('Sum1 "this" s1) ('Sum1 "that" s2)))
          a = do
            return $ Sum2Left $ Sum1Value (Proxy @"this") s1
          b = do
            return $ Sum2Right $ Sum1Value (Proxy @"that") s2
        s <- oneof [a, b]
        return $ SomeStructureValue $ SumValue s
      sum1 = do
        SomeStructureValue s <- resizeStruct arbitrary
        return $ SomeStructureValue $ SumValue $ Sum1Value (Proxy @"single") s
      prod2 = do
        SomeStructureValue (s1 :: StructureValue s1) <- resizeStruct arbitrary
        SomeStructureValue (s2 :: StructureValue s2) <- resizeStruct arbitrary
        return $ SomeStructureValue $ ProductValue $ Product2Value
          (Product1Value (Proxy @"some") s1)
          (Product1Value (Proxy @"other") s2)
      prod1 = do
        SomeStructureValue s <- resizeStruct arbitrary
        return $ SomeStructureValue $ ProductValue
          $ Product1Value (Proxy @"single") s
      prod0 = pure $ SomeStructureValue $ ProductValue Product0Value

-- | Turn all @Just Nothing@ to just @Nothing@. Needed because in json
-- we can not distinguish between @null@ and @just null@.
noJustNothing :: StructureValue s -> StructureValue s
noJustNothing a = go a
  where
    go :: StructureValue s -> StructureValue s
    go = \case
      OptionalValue (Just sub) -> case go sub of
        OptionalValue Nothing -> OptionalValue Nothing
        clear                 -> OptionalValue $ Just clear
      VectorValue v -> VectorValue $ go <$> v
      SumValue s -> SumValue $ goSum s
      ProductValue p -> ProductValue $ goProd p
      clear -> clear
    goSum :: SumTreeValue t -> SumTreeValue t
    goSum = \case
      Sum1Value p s -> Sum1Value p $ go s
      Sum2Left l -> Sum2Left $ goSum l
      Sum2Right r -> Sum2Right $ goSum r
    goProd :: ProductTreeValue t -> ProductTreeValue t
    goProd = \case
      Product0Value -> Product0Value
      Product1Value p s -> Product1Value p $ go s
      Product2Value p1 p2 -> Product2Value (goProd p1) (goProd p2)

test_StructureValue :: TestTree
test_StructureValue = testGroup "Pure tests"
  [ testProperty "ToJSON/FromJSON (StructureValue s)"
    $ \s -> case s of
      SomeStructureValue (v :: StructureValue s) ->
        Just (noJustNothing v) === J.decode (J.encode v)
  ]
