module Test.Tolstoy.Migrations where

import Control.DeepSeq
import Data.Typeable
import GHC.Exts
import GHC.TypeLits
import Test.QuickCheck
import Tolstoy.Migration

data Checks :: (* -> * -> Constraint) -> Nat -> [*] -> * where
  Check
    :: ( KnownNat n, dict a b, Typeable a, Typeable b )
    => Proxy n
    -> ((a -> b) -> Property)
    -- ^ The migration checker
    -> Checks dict (n + 1) (b ': rest)
    -> Checks dict n (a ': b ': rest)
  LastCheck :: (KnownNat n) => Checks dict n '[a]

class CheckAll (dict :: * -> * -> Constraint) n els where
  checkAll
    :: (forall a b. (dict a b) => (a -> b) -> Property)
    -> Checks dict n els

instance (KnownNat n) => CheckAll dict n '[a] where
  checkAll _ = LastCheck

instance
  ( CheckAll dict (n + 1) (b ': rest)
  , KnownNat n
  , dict a b
  , Typeable a, Typeable b
  ) => CheckAll dict n (a ': b ': rest) where
  checkAll chf = Check Proxy chf (checkAll chf)

class (Arbitrary a, Show a, NFData b) => TotalConstr a b
instance (Arbitrary a, Show a, NFData b) => TotalConstr a b

class AnyTypes a b
instance AnyTypes a b

allTotal :: (CheckAll TotalConstr n els) => Checks TotalConstr n els
allTotal = checkAll $ \f -> forAll arbitrary $ \a -> (total $ f a)

genMigrationsTests
  :: Migrations n els
  -> Checks dict n els
  -> [(String, Property)]
genMigrationsTests migs checks = case (migs, checks) of
  (Migrate n (f :: a -> b) rest, Check _ checkF checkRest) ->
    (name, checkF f)
    : genMigrationsTests rest checkRest
    where
      name = show (natVal n) ++ ": " ++ tn (Proxy @a) ++ " -> " ++ tn (Proxy @b)
      tn :: forall x. (Typeable x) => Proxy x -> String
      tn p = tyConName $ typeRepTyCon $ typeRep p
  (LastVersion _ _, LastCheck) -> []
