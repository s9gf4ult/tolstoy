module Tolstoy.Migration where

import           Data.Aeson as J
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Proxy
import           Data.Typeable
import           GHC.Generics (Generic)
import           GHC.TypeLits
import           Prelude as P
import           Tolstoy.Structure
import           Tolstoy.Types
import           TypeFun.Data.Peano

data Migrations :: N -> [*] -> * where
  Migrate
    :: ( Structural a, Structural b
       , Typeable b, KnownPeano (S n)
       , FromJSON (StructureValue (StructKind b))
       , KnownStructure (StructKind b)
       )
    => Proxy (S n)
    -> (a -> b)
    -> Migrations n (a ': rest)
    -> Migrations (S n) (b ': a ': rest)
  FirstVersion
    :: ( Structural a, Typeable a, KnownPeano n
       , FromJSON (StructureValue (StructKind a))
       , KnownStructure (StructKind a)
       )
    => Proxy n
    -> Proxy a
    -> Migrations n '[a]

type family Head (els :: [*]) where
  Head (a ': rest) = a

type MigMap a = Map Integer (Value -> TolstoyResult a)

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
        { version = peanoVal n
        , repValue = toJSON (structureRep :: StructureRep (StructKind typ)) }
      Migrate n (_f :: a -> b) rest -> this : versionsRep rest
        where
          this = VersionRep
            { version = peanoVal n
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
  Migrate n _ _ -> peanoVal n
  FirstVersion n _ -> peanoVal n

migMap
  :: forall n els
  .  Migrations n els
  -> MigMap (Head els)
migMap migs = M.fromList $ go P.id migs
  where
    go :: forall goEls goN
      .  (Head goEls -> Head els)
      -> Migrations goN goEls
      -> [(Integer, Value -> TolstoyResult (Head els))]
    go lift = \case
      FirstVersion pN (pA :: Proxy a) ->
        [(peanoVal pN, \v -> lift <$> aesonResult (fromJSON v))]
      Migrate pN (f :: a -> b) rest ->
        (peanoVal pN, \v -> lift <$> aesonResult (fromJSON v))
        : go (lift . f) rest

-- | Finds type the @Value@ should be parsed as, then applies migrations
-- to it. The result is the last type in the migrations list.
migrate
  :: forall r
  .  Integer
  -- ^ Version number of the value
  -> Value
  -- ^ The value itself
  -> MigMap r
  -- ^ Migrations to parse and migrate the value
  -> TolstoyResult r
migrate n v mm = case M.lookup n mm of
  Nothing -> Left $ VersionOutOfBounds n
  Just f  -> f v

aesonResult :: forall a. (Typeable a, Structural a)
  => J.Result (StructureValue (StructKind a))
  -> TolstoyResult a
aesonResult = \case
  J.Error a   -> Left $ AesonError (typeRep (Proxy @a)) a
  J.Success a -> Right $ fromStructValue a
