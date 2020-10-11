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
       , KnownNat (n + 1), Typeable b
       , FromJSON (StructureValue (StructKind b))
       , KnownStructure (StructKind b)
       )
    => Proxy (n + 1)
    -> (a -> b)
    -> Migrations n (a ': rest)
    -> Migrations (n + 1) (b ': a ': rest)
  FirstVersion
    :: ( KnownNat n, Structural a, Typeable a
       , FromJSON (StructureValue (StructKind a))
       , KnownStructure (StructKind a)
       )
    => Proxy n
    -> Proxy a
    -> Migrations n '[a]

type family Head (els :: [*]) where
  Head (a ': rest) = a


-- | Versions which must be inserted into the table before operation
newtype NeedsDeploy = NeedsDeploy
  { getNeedsDeploy :: [VersionRep]
  } deriving (Eq, Show, Generic)

-- | Returns versions in ascending order
versionsRep :: Migrations n els -> [VersionRep]
versionsRep = reverse . go
  where
    go :: Migrations n els -> [VersionRep]
    go = \case
      FirstVersion n (_typ :: Proxy typ) -> pure $ VersionRep
        { version = natVal n
        , repValue = toJSON (structureRep :: StructureRep (StructKind typ)) }
      Migrate n (_f :: a -> b) rest -> this : versionsRep rest
        where
          this = VersionRep
            { version = natVal n
            , repValue = toJSON (structureRep :: StructureRep (StructKind b)) }

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

actualMigrationIndex :: Migrations n els -> Integer
actualMigrationIndex = \case
  Migrate n _ _ -> natVal n
  FirstVersion n _ -> natVal n

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
  -> TolstoyResult (Head els)
migrate n v migrations = go P.id migrations
  where
    go :: forall goEls goN
      .  (Head goEls -> Head els)
      -> Migrations goN goEls
      -> TolstoyResult (Head els)
    go lift = \case
      FirstVersion pN (pA :: Proxy a) -> if natVal pN == n
        then lift <$> aesonResult (fromJSON v)
        else Left $ NoMoreVersions (typeRep pA) $ natVal pN
      Migrate pN (f :: a -> b) rest -> case compare n (natVal pN) of
        EQ -> lift <$> aesonResult (fromJSON v)
        LT -> go (lift . f) rest
        GT -> Left $ VersionOutOfBounds $ natVal pN

aesonResult :: forall a. (Typeable a, Structural a)
  => J.Result (StructureValue (StructKind a))
  -> TolstoyResult a
aesonResult = \case
  J.Error a   -> Left $ AesonError (typeRep (Proxy @a)) a
  J.Success a -> Right $ fromStructValue a
