module Tolstoy.DB where

import Control.Monad.Fail
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

type JSON a = (FromJSON a, ToJSON a, Typeable a)

data DocDesc doc act = DocDesc
  { doc      :: doc
  , docId    :: DocId
  , act      :: act
  , actId    :: ActId
  , seqnum   :: Seqnum
  , created  :: UTCTime
  , modified :: UTCTime
  } deriving (Eq, Ord, Show, Generic)

data Tolstoy m doc act = Tolstoy
  { newDoc
    :: doc
    -- ^ Initial state of the doc.
    -> act
    -- ^ Initial action. It will not be performed on given doc, only
    -- written to DB
    -> m (DocDesc doc act)
  -- ^ Inserts a new document in DB
  , getDoc
    :: DocId
    -> m (Maybe (DocDesc doc act))
  -- ^ Get last version of some object
  , changeDoc
    :: DocDesc doc act
    -> act
    -> m (Either Error (DocDesc doc act))
  -- ^ Saves changed doc to the DB. Note that it does not check the
  -- document history consistency right now.
  } deriving (Generic)

data TolstoyInit doc act = TolstoyInit
  { docAction      :: DocAction doc act
  , documentsTable :: FN
  , actionsTable   :: FN
  } deriving (Generic)

tolstoy
  :: forall m doc act
  .  ( MonadPostgres m
     , MonadFail m              -- FIXME: KILL it
     , JSON doc, JSON act)
  => (TolstoyInit doc act)
  -> Tolstoy m doc act
tolstoy init =
  Tolstoy { newDoc, getDoc, changeDoc }
  where
    docs = documentsTable
    newDoc doc act = do
      [(docId, created)] <- pgQuery
        [sqlExp|INSERT INTO ^{documentsTable init} RETURNING id, created_at|]
      [(actId, modified, seqnum)] <- pgQuery
        [sqlExp|INSERT INTO ^{actionsTable init} (document_id, document, action)
                VALUES ( #{docId}, #{JsonField doc}, #{JsonField act} )
                RETURNING id, created_at, seqnum|]
      let
        res = DocDesc { doc, docId, act, actId, seqnum, created, modified }
      return res
    getDoc docId = do
      res <- pgQuery
        [sqlExp|SELECT
        doc.id as doc_id,
        doc.created_at as created,
        act.id as act_id,
        act.created_at as modified,
        act.seqnum,
        act.document,
        act.action
        FROM ^{documentsTable init} as doc INNER JOIN ^{actionsTable init} as act
          ON doc.id = act.document_id
        WHERE doc.id = #{docId}
        |]
      case res of
        [] -> return Nothing
        [( docId
         , created
         , actId
         , modified
         , seqnum
         , JsonField doc
         , JsonField act)] -> return $ Just $ DocDesc {..}
    changeDoc docDesc act = case docAction init (doc docDesc) act of
      Left e -> return $ Left e
      Right newDoc -> do
        res <- pgQuery [sqlExp|
          INSERT INTO ^{actionsTable init} (document_id, parent_id, document, action)
          VALUES (#{docId docDesc}, #{actId docDesc}, #{JsonField newDoc}, #{JsonField act})
          RETURNING id, created_at, seqnum|]
        case res of
          [(newActId, newMod, newSeq)] -> return $ Right $ DocDesc
            { doc      = newDoc
            , docId    = docId docDesc
            , act      = act
            , actId    = newActId
            , seqnum   = newSeq
            , created  = created docDesc
            , modified = newMod
            }
