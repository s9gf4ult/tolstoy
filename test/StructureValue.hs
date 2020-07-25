module StructureValue where

import Control.Lens
import Control.Monad
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson as J
import Data.Generics.Product
import Data.List.NonEmpty as NE
import Data.Pool as Pool
import Data.Proxy
import Data.Scientific
import Data.Set as S
import Data.String
import Data.Text as T
import Data.Traversable
import Data.Vector as V
import Database.PostgreSQL.Query as PG
import Database.PostgreSQL.Simple as PG
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen as Gen
import Test.Tasty as Test
import Test.Tasty.HUnit as Test
import Test.Tasty.QuickCheck
import Tolstoy.DB
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
        [ SomeStructureValue . StringValue <$> arbText
        , SomeStructureValue . NumberValue <$> arbScientific
        , SomeStructureValue . BoolValue <$> arbitrary
        , opt
        , vec
        , sum
        , prod
        ]
      opt = do
        SomeStructureValue s <- arbitrary
        sub <- oneof [ pure $ Just s, pure Nothing ]
        return $ SomeStructureValue $ OptionalValue sub
      vec = do
        SomeStructureValue (_ :: StructureValue s) <- arbitrary
        vec <- arbitrary :: Gen (Vector (StructureValue s))
        return $ SomeStructureValue $ VectorValue vec
      sum = do
        SomeStructureValue (_ :: StructureValue s1) <- arbitrary
        SomeStructureValue (_ :: StructureValue s2) <- arbitrary
        sum <- arbitrary :: Gen (SumValueL '[ '("some", s1), '("other", s2) ])
        return $ SomeStructureValue $ SumValue sum
      prod = do
        SomeStructureValue (_ :: StructureValue s1) <- arbitrary
        SomeStructureValue (_ :: StructureValue s2) <- arbitrary
        p <- arbitrary ::
          Gen (ProductValueL '[ '("some", s1), '("other", s2) ])
        return $ SomeStructureValue $ ProductValue p

test_StructureValue :: TestTree
test_StructureValue = testGroup "Pure tests"
  [ testProperty "ToJSON/FromJSON (StructureValue s)"
    $ \(s :: SomeStructureValue) -> case s of
      SomeStructureValue (v :: StructureValue s) ->
        Just v === J.decode (J.encode v)
  ]
