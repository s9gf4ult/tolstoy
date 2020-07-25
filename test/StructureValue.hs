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
    , ToJSON (StructureValue s)
    , Eq (StructureValue s)
    , Show (StructureValue s)
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
      allCases = optNothing : noNothing
      noNothing =
        [ SomeStructureValue . StringValue <$> arbText
        , SomeStructureValue . NumberValue <$> arbScientific
        , SomeStructureValue . BoolValue <$> arbitrary
        , optJust
        ]
      optJust = do
        SomeStructureValue s <- oneof noNothing
        -- Just Nothing will be parsed like Nothing, so we omit those
        -- cases
        return $ SomeStructureValue $ OptionalValue $ Just s
      optNothing = do
        SomeStructureValue (_ :: StructureValue s) <- arbitrary
        return $ SomeStructureValue $ OptionalValue
          (Nothing :: Maybe (StructureValue s))


test_StructureValue :: TestTree
test_StructureValue = testGroup "Pure tests"
  [ testProperty "ToJSON/FromJSON (StructureValue s)"
    $ \(s :: SomeStructureValue) -> case s of
      SomeStructureValue (v :: StructureValue s) ->
        Just v == J.decode (J.encode v)
  ]
