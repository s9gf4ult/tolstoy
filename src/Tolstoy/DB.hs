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

type JSON a = (FromJSON a, ToJSON a, Typeable a)

data DocDesc doc act = DocDesc
  { doc      :: JsonField doc
  , docId    :: DocId doc
  , act      :: JsonField act
  , actId    :: ActId act
  , created  :: UTCTime
  , modified :: UTCTime
  } deriving (Eq, Ord, Show, Generic)

instance (JSON doc, JSON act) => FromRow (DocDesc doc act)

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
  , changeDoc
    :: DocDesc doc act
    -> act
    -> m (Either Error ((DocDesc doc act), a))
  -- ^ Saves changed doc to the DB. Note that it does not check the
  -- document history consistency right now.
  , documentsList :: SqlBuilder
  } deriving (Generic)

data TolstoyInit doc act a = TolstoyInit
  { docAction      :: DocAction doc act a
  , documentsTable :: FN
  , actionsTable   :: FN
  } deriving (Generic)

tolstoy
  :: forall m doc act a
  .  ( MonadPostgres m
     , MonadFail m              -- FIXME: KILL it
     , JSON doc, JSON act)
  => (TolstoyInit doc act a)
  -> Tolstoy m doc act a
tolstoy init =
  Tolstoy { newDoc, getDoc, changeDoc, documentsList }
  where
    docs = documentsTable
    documentsList = [sqlExp|SELECT
      act.document as doc,
      doc.id as doc_id,
      act.action as act,
      act.id ad act_id,
      doc.created_at as created,
      act.created_at as modified
      FROM ^{documentsTable init} as doc
        INNER JOIN ^{actionsTable init} as act ON doc.action_id = act.id
      |]
    newDoc doc act = do
      [Only actId] <- pgQuery [sqlExp|
        INSERT INTO ^{actionsTable init} (document, action)
        VALUES ( #{JsonField doc}, #{JsonField act} )
        RETURNING id|]
      [(docId, created)] <- pgQuery [sqlExp|
        INSERT INTO ^{documentsTable init} (action_id)
        VALUES ( #{actId} )
        RETURNING id, created_at|]
      let
        res = DocDesc
          { doc = JsonField doc
          , docId
          , act = JsonField act
          , actId
          , created
          , modified = created }
      return res
    getDoc docId = do
      res <- pgQuery [sqlExp|
        SELECT * FROM (^{documentsList}) where doc_id = #{docId}|]
      case res of
        []        -> return Nothing
        [docDesc] -> return $ Just docDesc
        _         -> error "Unexpected count of results"
    changeDoc docDesc act = case docAction init (unJsonField $ doc docDesc) act of
      Left e       -> return $ Left e
      Right (newDoc, a) -> do
        [(newActId, newMod)] <- pgQuery [sqlExp|
          INSERT INTO ^{actionsTable init} (parent_id, document, action)
          VALUES ( #{actId docDesc}, #{JsonField newDoc}, #{JsonField act})
          RETURNING id, created_at|]
        1 <- pgExecute [sqlExp|UPDATE ^{documentsTable init}
          SET action_id = #{newActId}
          WHERE id = #{docId docDesc}|]
        let
          res =  DocDesc
            { doc      = JsonField newDoc
            , docId    = docId docDesc
            , act      = JsonField act
            , actId    = newActId
            , created  = created docDesc
            , modified = newMod }
        return $ Right (res, a)
