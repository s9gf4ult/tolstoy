module Tolstoy.DB where

import Data.Aeson
import Data.Pool
import Data.Text as T
import Data.Time
import Data.Typeable
import Data.UUID.Types
import Database.PostgreSQL.Query as PG
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics (Generic)
import GHC.Stack

type Error = Text

type DocAction doc act = doc -> act -> Either Error doc

newtype Seqnum = Seqnum
  { unSeqnum :: Integer
  } deriving (Eq, Ord, Show, FromField, ToField, Generic)

newtype DocId = DocId
  { unDocId :: UUID
  } deriving (Eq, Ord, Show, Generic, FromField, ToField, FromJSON, ToJSON)

newtype ActId = ActId
  { unActId :: UUID
  } deriving (Eq, Ord, Show, Generic, FromField, ToField, FromJSON, ToJSON)

newtype JsonField a = JsonField
  { unJsonField :: a
  } deriving (Eq, Ord, Show, Generic)

instance (FromJSON a, Typeable a) => FromField (JsonField a) where
  fromField a b = JsonField <$> fromJSONField a b

instance (ToJSON a) => ToField (JsonField a) where
  toField = toJSONField . unJsonField

data DocDesc doc act = DocDesc
  { doc      :: doc
  , docId    :: DocId
  , act      :: act
  , actId    :: ActId
  , seqnum   :: Seqnum
  , created  :: UTCTime
  , modified :: UTCTime
  } deriving (Eq, Ord, Show, Generic)

data Tolstoy doc act = Tolstoy
  { newDoc
    :: doc
    -- ^ Initial state of the doc.
    -> act
    -- ^ Initial action. It will not be performed on given doc, only
    -- written to DB
    -> IO (DocDesc doc act)
  -- ^ Inserts a new document in DB
  , getDoc
    :: DocId
    -> IO (Maybe (DocDesc doc act))
  -- ^ Get last version of some object
  , changeDoc
    :: DocDesc doc act
    -> act
    -> IO (Either Error (DocDesc doc act))
  -- ^ Saves changed doc to the DB. Note that it does not check the
  -- document history consistency right now.
  } deriving (Generic)

data TolstoyInit doc act = TolstoyInit
  { docAction      :: DocAction doc act
  , dbPool         :: Pool PG.Connection
  , documentsTable :: FN
  , actionsTable   :: FN
  } deriving (Generic)

runPG
  :: TolstoyInit doc act
  -> (HasCallStack => PgMonadT IO a)
  -> IO a
runPG = error "FIXME: runPG not implemented"

tolstoy
  :: forall doc act
  .  (TolstoyInit doc act)
  -> Tolstoy doc act
tolstoy init =
  Tolstoy { newDoc, getDoc, changeDoc }
  where
    docs = documentsTable
    newDoc doc act = runPG init $ do
      [(docId, created)] <- pgQuery
        [sqlExp|INSERT INTO ^{documentsTable init} RETURNING id, created_at|]
      [(actId, modified, seqnum)] <- pgQuery
        [sqlExp|INSERT INTO ^{actionsTable init} (document_id, document, action)
                VALUES ( #{docId}, #{JsonField doc}, #{JsonField act} )
                RETURNING id, created_at, seqnum|]
      let
        res = DocDesc {doc, docId, act, actId, seqnum, created, modified }
      return res

    getDoc = (error "FIXME: not implemented")
    changeDoc = (error "FIXME: not implemented")
