module Tolstoy.Migration where

import Data.Aeson as J
import Data.Proxy
import Data.Typeable
import Data.UUID.Types
import GHC.Generics (Generic)
import GHC.TypeLits
import Tolstoy.Structure

data Migrations :: Nat -> [*] -> * where
  Migrate
    :: ( Structural a, Structural b
       , KnownNat n, Typeable a
       , FromJSON (StructureValue (StructKind a)) )
    => Proxy n
    -> (a -> b)
    -> Migrations (n + 1) (b ': rest)
    -> Migrations n (a ': b ': rest)
  LastVersion
    :: ( KnownNat n, Structural a, Typeable a
       , FromJSON (StructureValue (StructKind a)) )
    => Proxy n
    -> Proxy a
    -> Migrations n '[a]

type MigrationResult = Either MigrationError

data MigrationError
  = AesonError TypeRep String
  | NoMoreVersions TypeRep Integer
  | VersionOutOfBounds Integer
  | ActionNotFound UUID
  deriving (Eq, Ord, Show, Generic)

type family Last (els :: [*]) where
  Last (a ': b ': rest) = Last (b ': rest)
  Last '[a]             = a

lastVersion :: Migrations n els -> Integer
lastVersion = \case
  Migrate _ _ rest -> lastVersion rest
  LastVersion n _  -> natVal n

-- | Finds type the @Value@ should be parsed as, then applies migrations
-- to it. The result is the last type in the migrations list.
migrate
  :: forall n els
  .  Integer
  -- ^ Version number of the value
  -> Value
  -- ^ The value itself
  -> Migrations n els
  -- ^ Migrations to parse and migrate the value
  -> MigrationResult (Last els)
migrate n v migrations = case migrations of
  LastVersion pN (_ :: Proxy a) -> if natVal pN == n
    then up <$> aesonResult (fromJSON v)
    else Left $ NoMoreVersions (typeRep (Proxy @a)) $ natVal pN
    where
      up :: a -> a
      up = id
  Migrate pN (_ :: a -> b) rest -> case compare n (natVal pN) of
    GT -> migrate n v rest
    EQ -> up <$> aesonResult (fromJSON v)
    LT -> Left $ VersionOutOfBounds $ natVal pN
    where
      up :: a -> Last els
      up a = applyMigrations a migrations

applyMigrations :: a -> Migrations n (a ': rest) -> Last (a ': rest)
applyMigrations a = \case
  LastVersion _ _  -> a
  Migrate _ f rest -> applyMigrations (f a) rest

aesonResult :: forall a. (Typeable a, Structural a)
  => J.Result (StructureValue (StructKind a))
  -> MigrationResult a
aesonResult = \case
  J.Error a   -> Left $ AesonError (typeRep (Proxy @a)) a
  J.Success a -> Right $ fromStructValue a


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
