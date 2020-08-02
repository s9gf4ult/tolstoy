module Test.Tolstoy.Migrations where

import Control.DeepSeq
import Data.Typeable
import GHC.TypeLits
import Test.QuickCheck
import Tolstoy.Migration

type family ElsTestable els where
  ElsTestable (a ': b ': rest) =
    ( Arbitrary a, Show a, Typeable a
    , NFData b, Typeable b
    , ElsTestable (b ': rest) )
  ElsTestable '[a] = NFData a

genMigrationsTests
  :: (ElsTestable els)
  => Migrations n els
  -> [(String, Property)]
genMigrationsTests = \case
  LastVersion _ _  -> []
  Migrate n f rest -> (name, forAll arbitrary $ \a -> total $ f a )
    : genMigrationsTests rest
    where
      name = show (natVal n) ++ ": " ++ (tyConName $ typeRepTyCon $ typeOf f)
