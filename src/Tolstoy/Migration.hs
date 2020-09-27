module Tolstoy.Migration where

import Data.Aeson as J
import Data.Proxy
import Data.Typeable
import GHC.Generics (Generic)
import GHC.TypeLits
import Prelude as P
import Tolstoy.Structure
import Tolstoy.Types

data Migrations :: Nat -> [*] -> * where
  Migrate
    :: ( Structural a, Structural b
       , KnownNat n, Typeable a
       , FromJSON (StructureValue (StructKind a))
       , KnownStructure (StructKind a)
       )
    => Proxy n
    -> (a -> b)
    -> Migrations (n + 1) (b ': rest)
    -> Migrations n (a ': b ': rest)
  LastVersion
    :: ( KnownNat n, Structural a, Typeable a
       , FromJSON (StructureValue (StructKind a))
       , KnownStructure (StructKind a)
       )
    => Proxy n
    -> Proxy a
    -> Migrations n '[a]


type family Last (els :: [*]) where
  Last (a ': b ': rest) = Last (b ': rest)
  Last '[a]             = a


-- | Versions which must be inserted into the table before operation
newtype NeedsDeploy = NeedsDeploy
  { getNeedsDeploy :: [VersionRep]
  } deriving (Eq, Show, Generic)

versionsRep :: Migrations n els -> [VersionRep]
versionsRep = \case
  LastVersion n (_typ :: Proxy typ) -> pure $ VersionRep
    { version = natVal n
    , repValue = toJSON (structureRep :: StructureRep (StructKind typ)) }
  Migrate n (_f :: a -> b) rest -> this : versionsRep rest
    where
      this = VersionRep
        { version = natVal n
        , repValue = toJSON (structureRep :: StructureRep (StructKind a)) }

checkVersions
  :: Migrations n els
  -- ^ Migrations with versions the app expects
  -> [VersionRep]
  -- ^ The versions deployed in the database in ascending order. The
  -- oldest versions first
  -> TolstoyResult NeedsDeploy
checkVersions migs dbVs' = fmap NeedsDeploy $ go dbVs' $ versionsRep migs
  where
    go [] []         = return []
    go dbVs@(_:_) [] = Left $ DatabaseIsNewerThanApp dbVs
    go [] migVs@(_:_) = return migVs
    go (dbV:dbRest) (migV:migRest) = if dbV == migV
      then go dbRest migRest
      else Left $ DatabaseHasIncompatibleMigration dbV migV

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
  -> TolstoyResult (Last els)
migrate n v migrations = case migrations of
  LastVersion pN (_ :: Proxy a) -> if natVal pN == n
    then up <$> aesonResult (fromJSON v)
    else Left $ NoMoreVersions (typeRep (Proxy @a)) $ natVal pN
    where
      up :: a -> a
      up = P.id
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
  -> TolstoyResult a
aesonResult = \case
  J.Error a   -> Left $ AesonError (typeRep (Proxy @a)) a
  J.Success a -> Right $ fromStructValue a
