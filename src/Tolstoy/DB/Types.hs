module Tolstoy.DB.Types where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.Except
import           Data.Aeson
import           Data.Char as C
import           Data.Generics.Product
import           Data.List.NonEmpty as NE
import qualified Data.Map as M
import           Data.Text as T
import           Data.Time
import           Data.Typeable
import           Data.UUID.Types
import           Database.PostgreSQL.Query as PG
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           GHC.Generics (Generic)
import           Prelude as P
import           Tolstoy.Migration
import           Tolstoy.Structure


type Error = Text

type DocAction doc act a = doc -> act -> Either Error (doc, a)

type PureDocAction doc act = DocAction doc act ()

pureDocAction :: (doc -> act -> Either Error doc) -> PureDocAction doc act
pureDocAction f doc act = (,()) <$> f doc act

newtype Seqnum = Seqnum
  { unSeqnum :: Integer
  } deriving (Eq, Ord, Show, FromField, ToField, Generic)

newtype DocId doc = DocId
  { unDocId :: UUID
  } deriving (Eq, Ord, Show, Generic, FromField, ToField, FromJSON, ToJSON)

newtype ActId act = ActId
  { unActId :: UUID
  } deriving (Eq, Ord, Show, Generic, FromField, ToField, FromJSON, ToJSON)

newtype JsonField a = JsonField
  { unJsonField :: a
  } deriving (Eq, Ord, Show, Generic)

instance (FromJSON a, Typeable a) => FromField (JsonField a) where
  fromField a b = JsonField <$> fromJSONField a b

instance (ToJSON a) => ToField (JsonField a) where
  toField = toJSONField . unJsonField

type StructuralJSON doc =
  ( Structural doc
  , Typeable (StructureValue (StructKind doc))
  , FromJSON (StructureValue (StructKind doc))
  , ToJSON (StructureValue (StructKind doc)) )

data Doctype = Document | Action
  deriving (Eq, Ord, Show, Generic)

derivePgEnum (fmap C.toLower) ''Doctype

data VersionRaw = VersionRaw
  { id            :: !UUID
  , doctype       :: !Doctype
  , version       :: !Integer
  , structure_rep :: !Value
  , created_at    :: !UTCTime
  } deriving (Eq, Show, Generic)

data DocDesc doc act = DocDesc
  { document        :: !doc
  , documentId      :: !(DocId doc)
  , documentVersion :: !Integer
  -- ^ Original version number before migration
  , action          :: !act
  , actionId        :: !(ActId act)
  , actionVersion   :: !Integer
  -- ^ Original version number before migration
  , created         :: !UTCTime
  , modified        :: !UTCTime
  } deriving (Eq, Ord, Show, Generic)

-- | Convenient type to parse DocDesc from sql
data DocDescRaw = DocDescRaw
  { document        :: JsonField Value
  , documentId      :: UUID
  , documentVersion :: Integer
  , action          :: JsonField Value
  , actionId        :: UUID
  , actionVersion   :: Integer
  , created         :: UTCTime
  , modified        :: UTCTime
  } deriving (Eq, Show, Generic)

instance FromRow DocDescRaw

migrateDocDesc
  :: forall n1 docs n2 acts doc act
  .  (doc ~ Last docs, act ~ Last acts)
  => Migrations n1 docs
  -> Migrations n2 acts
  -> DocDescRaw
  -> TolstoyResult (DocDesc doc act)
migrateDocDesc docMigs actMigs raw = do
  document <- migrate
    (raw ^. field @"documentVersion")
    (unJsonField $ raw ^. field @"document")
    docMigs
  action <- migrate
    (raw ^. field @"actionVersion")
    (unJsonField $ raw ^. field @"action")
    actMigs
  return $ DocDesc
    { document
    , documentId = DocId $ raw ^. field @"documentId"
    , documentVersion = raw ^. field @"documentVersion"
    , action
    , actionId = ActId $ raw ^. field @"actionId"
    , actionVersion = raw ^. field @"actionVersion"
    , created = raw ^. field @"created"
    , modified = raw ^. field @"modified"
    }

data DocHistory doc act = DocHistory
  { documentId :: !(DocId doc)
  , created    :: !UTCTime
  , history    :: !(NonEmpty (Story doc act))
  -- ^ Story points in reverse order. Head is the last actual version
  -- and tail is the initial
  } deriving (Eq, Ord, Show, Generic)

data Story doc act = Story
  { document        :: !doc
  , documentVersion :: !Integer
  -- ^ Original document version number before migration
  , action          :: !act
  , actionId        :: !(ActId act)
  , actionVersion   :: !Integer
  -- ^ Original action version number before migration
  , modified        :: !UTCTime
  , parentId        :: !(Maybe (ActId act))
  } deriving (Eq, Ord, Show, Generic)

data ActionRaw = ActionRaw
  { document        :: JsonField Value
  , documentVersion :: Integer
  , action          :: JsonField Value
  , actionId        :: UUID
  , actionVersion   :: Integer
  , modified        :: UTCTime
  , parentId        :: Maybe UUID
  } deriving (Eq, Show, Generic)

instance FromRow ActionRaw

actionsHistory
  :: forall n1 docs n2 acts doc act
  . (act ~ Last acts, doc ~ Last docs)
  => Migrations n1 docs
  -> Migrations n2 acts
  -> ActId act
  -> [ActionRaw]
  -> TolstoyResult [Story doc act]
actionsHistory docMigs actMigs a actions = go $ unActId a
  where
    go :: UUID -> TolstoyResult [Story doc act]
    go actId = case M.lookup actId actMap of
      Nothing  -> throwError $ ActionNotFound actId
      Just raw -> do
        h <- toStory raw
        rest <- case raw ^. field @"parentId" of
          Nothing     -> pure []
          Just parent -> go parent
        return $ h : rest
    toStory raw = do
      document <- migrate
        (raw ^. field @"documentVersion")
        (raw ^. field @"document" . to unJsonField)
        docMigs
      action <- migrate
        (raw ^. field @"actionVersion")
        (raw ^. field @"action" . to unJsonField)
        actMigs
      return $ Story
        { document
        , documentVersion = raw ^. field @"documentVersion"
        , action
        , actionId        = raw ^. field @"actionId" . to ActId
        , actionVersion   = raw ^. field @"actionVersion"
        , modified        = raw ^. field @"modified"
        , parentId        = raw ^? field @"parentId" . _Just . to ActId }
    actMap :: M.Map UUID ActionRaw
    actMap = M.fromList $ (getField @"actionId" &&& P.id) <$> actions

data TolstoyInit doc act a = TolstoyInit
  { docAction       :: !(DocAction doc act a)
  , documentsTable  :: !FN
  , actionsTable    :: !FN
  , versionsTable   :: !FN
  , doctypeTypeName :: !FN
  } deriving (Generic)

data Tolstoy m doc act a = Tolstoy
  { newDoc
    :: doc
    -- ^ Initial state of the doc.
    -> act
    -- ^ Initial action. It will not be performed on given doc, only
    -- written to DB
    -> m (TolstoyResult (DocDesc doc act))
  -- ^ Inserts a new document in DB
  , getDoc
    :: DocId doc
    -> m (Maybe (TolstoyResult (DocDesc doc act)))
  -- ^ Get last version of some object
  , getDocHistory
    :: DocId doc
    -> m (Maybe (TolstoyResult (DocHistory doc act)))
  -- ^ Get full history of the document
  , changeDoc
    :: DocDesc doc act
    -> act
    -> m (TolstoyResult ((DocDesc doc act), a))
  -- ^ Saves changed doc to the DB. Note that it does not check the
  -- document history consistency from the business logic perspective
  , listDocuments :: m (TolstoyResult [DocDesc doc act])
  } deriving (Generic)

data TolstoyQueries doc act = TolstoyQueries
  { deploy        :: SqlBuilder
  -- ^ Deploy tables to the database
  , revert        :: SqlBuilder
  -- ^ Revert tables (drop em)
  , documentsList :: SqlBuilder
  -- ^ List of latest versions of documents
  , actionsList   :: ActId act -> SqlBuilder
  -- ^ Get action id to get started from
  } deriving (Generic)
