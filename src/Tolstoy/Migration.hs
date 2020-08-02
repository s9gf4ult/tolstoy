module Tolstoy.Migration where

import Data.Aeson as J
import Data.Proxy
import GHC.Generics (Generic)
import GHC.TypeLits
import Tolstoy.Structure

data Migrations :: Nat -> [*] -> * where
  Migrate
    :: ( Structural a, Structural b
       , KnownNat n
       , FromJSON (StructureValue (StructKind a)) )
    => Proxy n
    -> (a -> b)
    -> Migrations (n + 1) (b ': rest)
    -> Migrations n (a ': b ': rest)
  LastVersion
    :: ( KnownNat n, Structural a
       , FromJSON (StructureValue (StructKind a)) )
    => Proxy n
    -> Proxy a
    -> Migrations n '[a]

type MigrationResult = Either MigrationError

data MigrationError
  = AesonError String
  | NoMoreVersions Integer
  | VersionOutOfBounds Integer
  deriving (Eq, Ord, Show, Generic)

type family Last (els :: [*]) where
  Last (a ': b ': rest) = Last (b ': rest)
  Last '[a]             = a

migrate
  :: forall n els
  .  Integer
  -> Value
  -> Migrations n els
  -> MigrationResult (Last els)
migrate n v migrations = case migrations of
  LastVersion pN (_ :: Proxy a) -> if natVal pN == n
    then aesonResult $ up <$> fromJSON v
    else Left $ NoMoreVersions $ natVal pN
    where
      up :: StructureValue (StructKind a) -> a
      up = fromStructValue
  Migrate pN (_ :: a -> b) rest -> case compare n (natVal pN) of
    GT -> migrate n v rest
    EQ -> aesonResult $ up <$> fromJSON v
    LT -> Left $ VersionOutOfBounds $ natVal pN
    where
      up :: StructureValue (StructKind a) -> Last els
      up a = applyMigrations (fromStructValue a) migrations

applyMigrations :: a -> Migrations n (a ': rest) -> Last (a ': rest)
applyMigrations a = \case
  LastVersion _ _  -> a
  Migrate _ f rest -> applyMigrations (f a) rest

aesonResult :: J.Result a -> MigrationResult a
aesonResult = \case
  J.Error a   -> Left $ AesonError a
  J.Success a -> Right a


-- class Migrate (n :: Nat) (els :: [*]) (a :: *) (result :: *) where
--   migrate :: Migrations n els -> a -> result

-- instance Migrate n ('[a]) a a where
--   migrate (LastVersion _proxy) a = a

-- instance
--   ( Migrate (n + 1) (b ': rest) b result
--   ) => Migrate n (a ': b ': rest) a result where
--   migrate (Migrate _proxy f rest) a = migrate rest (f a)

-- instance Migrate n (x ': b ': rest) a result where
--   migrate (Migrate _proxy _f rest) a = migrate
