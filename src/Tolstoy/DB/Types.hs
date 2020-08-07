module Tolstoy.DB.Types where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.Fail
import           Data.Aeson
import           Data.Generics.Product
import           Data.List.NonEmpty as NE
import qualified Data.Map as M
import           Data.Pool
import           Data.Text as T
import           Data.Time
import           Data.Typeable
import           Data.UUID.Types
import           Database.PostgreSQL.Query as PG
import           Database.PostgreSQL.Simple.FromField
import qualified Database.PostgreSQL.Simple.FromRow as PG
import           Database.PostgreSQL.Simple.ToField
import           GHC.Generics (Generic)
import           GHC.Stack
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

data DocDesc doc act = DocDesc
  { doc      :: doc
  , docId    :: DocId doc
  , act      :: act
  , actId    :: ActId act
  , created  :: UTCTime
  , modified :: UTCTime
  } deriving (Eq, Ord, Show, Generic)

instance
  (StructuralJSON doc, StructuralJSON act
  ) => FromRow (DocDesc doc act) where
  fromRow = do
    JsonField docValue <- PG.field
    let doc = fromStructValue docValue
    docId <- PG.field
    JsonField actValue <- PG.field
    let act = fromStructValue actValue
    actId <- PG.field
    created <- PG.field
    modified <- PG.field
    return $ DocDesc {..}

data DocHistory doc act = DocHistory
  { docId   :: DocId doc
  , created :: UTCTime
  , history :: NonEmpty (Story doc act)
  -- ^ Story points in reverse order. Head is the last actual version
  -- and tail is the initial
  } deriving (Eq, Ord, Show, Generic)

data Story doc act = Story
  { doc      :: doc
  , act      :: act
  , actId    :: ActId act
  , modified :: UTCTime
  } deriving (Eq, Ord, Show, Generic)

data ActionRow doc act = ActionRow
  { actionId :: ActId act
  , created  :: UTCTime
  , parentId :: Maybe (ActId act)
  , document :: doc
  , action   :: act
  } deriving (Eq, Ord, Show, Generic)

instance
  ( StructuralJSON doc, StructuralJSON act
  ) => FromRow (ActionRow doc act) where
  fromRow = do
    actionId <- PG.field
    created <- PG.field
    parentId <- PG.field
    JsonField docVal  <- PG.field
    JsonField actVal <- PG.field
    let
      document = fromStructValue docVal
      action = fromStructValue actVal
    return $ ActionRow {..}

data TolstoyInit doc act a = TolstoyInit
  { docAction      :: DocAction doc act a
  , documentsTable :: FN
  , actionsTable   :: FN
  , versionsTable  :: FN
  } deriving (Generic)

data Tolstoy m doc act a = Tolstoy
  { newDoc
    :: doc
    -- ^ Initial state of the doc.
    -> act
    -- ^ Initial action. It will not be performed on given doc, only
    -- written to DB
    -> m (DocDesc doc act)
  -- ^ Inserts a new document in DB
  , getDoc
    :: DocId doc
    -> m (Maybe (DocDesc doc act))
  -- ^ Get last version of some object
  , getDocHistory
    :: DocId doc
    -> m (Maybe (DocHistory doc act))
  -- ^ Get full history of the document
  , changeDoc
    :: DocDesc doc act
    -> act
    -> m (Either Error ((DocDesc doc act), a))
  -- ^ Saves changed doc to the DB. Note that it does not check the
  -- document history consistency right now.
  , listDocuments :: m [DocDesc doc act]
  -- ^ List all docs in DB. Might be not very useful in practice
  , queries :: TolstoyQueries doc act
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
