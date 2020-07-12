module Tolstoy.DB where

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
  { doc      :: doc
  , docId    :: DocId doc
  , act      :: act
  , actId    :: ActId act
  , created  :: UTCTime
  , modified :: UTCTime
  } deriving (Eq, Ord, Show, Generic)

instance (JSON doc, JSON act) => FromRow (DocDesc doc act) where
  fromRow = do
    JsonField doc <- PG.field
    docId <- PG.field
    JsonField act <- PG.field
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
  , parentId :: ActId act
  , document :: doc
  , action   :: act
  } deriving (Eq, Ord, Show, Generic)

instance (JSON doc, JSON act) => FromRow (ActionRow doc act) where
  fromRow = do
    actionId <- PG.field
    created <- PG.field
    parentId <- PG.field
    JsonField document <- PG.field
    JsonField action <- PG.field
    return $ ActionRow {..}

actionsHistory
  :: forall doc act
  .  ActId act
  -> [ActionRow doc act]
  -> Maybe (NonEmpty (Story doc act))
actionsHistory a actions = NE.nonEmpty $ go a
  where
    go actId = case M.lookup actId actMap of
      Nothing -> []
      Just h  -> toStory h : (go $ h ^. field @"parentId")
    toStory h = Story
      { doc = h ^. field @"document"
      , act = h ^. field @"action"
      , actId = h ^. field @"actionId"
      , modified = h ^. field @"created" }
    actMap :: M.Map (ActId act) (ActionRow doc act)
    actMap = M.fromList $ (view (field @"actionId") &&& id) <$> actions

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
  Tolstoy { newDoc, getDoc, getDocHistory, changeDoc, documentsList }
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
          { doc , docId , act , actId , created
          , modified = created }
      return res
    getDoc docId = do
      res <- pgQuery [sqlExp|
        SELECT * FROM (^{documentsList}) where doc_id = #{docId}|]
      case res of
        []        -> return Nothing
        [docDesc] -> return $ Just docDesc
        _         -> error "Unexpected count of results"
    getDocHistory docId = do
      res <- pgQuery [sqlExp|SELECT created_at, action_id
        FROM ^{documentsTable init}
        WHERE id = #{docId}|]
      case res of
        [] -> return Nothing
        [(created, actId)] -> do
          actions <- pgQuery [sqlExp|
            WITH RECURSIVE result(id, created_at, parrent_id, document, action) AS (
              SELECT id, created_at, parrent_id, document, action
              FROM ^{actionsTable init}
              WHERE id = #{actId}
            UNION ALL
              SELECT act.id, act.created_at, act.parrent_id, act.document, act.action
              FROM ^{actionsTable init} AS act INNER JOIN result AS res
                ON res.parrent_id = act.id
            ) SELECT * FROM result |]
          case actionsHistory actId actions of
            Nothing      -> error "No actions. Unexpected result"
            Just history -> return $ Just
              $ DocHistory {docId, created, history}

    changeDoc docDesc act = case docAction init (docDesc ^. field @"doc") act of
      Left e       -> return $ Left e
      Right (newDoc, a) -> do
        let
          parent = docDesc ^. field @"actId"
          docId = docDesc ^. field @"docId"
        [(newActId, newMod)] <- pgQuery [sqlExp|
          INSERT INTO ^{actionsTable init} (parent_id, document, action)
          VALUES ( #{parent}, #{JsonField newDoc}, #{JsonField act})
          RETURNING id, created_at|]
        1 <- pgExecute [sqlExp|UPDATE ^{documentsTable init}
          SET action_id = #{newActId}
          WHERE id = #{docId}|]
        let
          res =  DocDesc
            { doc      = newDoc
            , docId    = docId
            , act      = act
            , actId    = newActId
            , created  = docDesc ^. field @"created"
            , modified = newMod }
        return $ Right (res, a)
