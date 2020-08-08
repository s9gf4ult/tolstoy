module Tolstoy.DB.Init where

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
import           Tolstoy.DB.Types
import           Tolstoy.Structure

actionsHistory
  :: forall doc act
  .  ActId act
  -> [ActionRow doc act]
  -> Maybe (NonEmpty (Story doc act))
actionsHistory a actions = NE.nonEmpty $ go a
  where
    go actId = case M.lookup actId actMap of
      Nothing -> []
      Just h  -> toStory h
        : (h ^.. field @"parentId" . _Just . to go . traversed)
    toStory h = Story
      { doc = h ^. field @"document"
      , act = h ^. field @"action"
      , actId = h ^. field @"actionId"
      , modified = h ^. field @"created" }
    actMap :: M.Map (ActId act) (ActionRow doc act)
    actMap = M.fromList $ (view (field @"actionId") &&& id) <$> actions

initQueries :: TolstoyInit doc act a -> TolstoyQueries doc act
initQueries init = TolstoyQueries { deploy, revert, documentsList, actionsList }
  where
    documents = documentsTable init
    actions = actionsTable init
    versions = versionsTable init
    documentsList = $(sqlExpFile "documentsList")
    deploy = $(sqlExpFile "deploy")
    revert = $(sqlExpFile "revert")
    actionsList actId = $(sqlExpFile "actionsList")

tolstoy
  :: forall m doc act a
  .  ( MonadPostgres m
     , MonadFail m              -- FIXME: KILL it
     , StructuralJSON doc, StructuralJSON act
     , HasCallStack
     )
  => (TolstoyInit doc act a)
  -> Tolstoy m doc act a
tolstoy init =
  Tolstoy { newDoc, getDoc, getDocHistory, changeDoc, listDocuments, queries }
  where
    docs = documentsTable
    queries = initQueries init
    newDoc doc act = do
      [(actId, modified)] <- pgQuery [sqlExp|
        INSERT INTO ^{actionsTable init} (document, action)
        VALUES ( #{JsonField (toStructValue doc)}
          , #{JsonField (toStructValue act)} )
        RETURNING id, created_at|]
      [(docId, created)] <- pgQuery [sqlExp|
        INSERT INTO ^{documentsTable init} (action_id)
        VALUES ( #{actId} )
        RETURNING id, created_at|]
      let
        res = DocDesc
          { doc , docId , act , actId , created, modified }
      return res
    getDoc docId = do
      res <- pgQuery [sqlExp|
        SELECT * FROM (^{documentsList queries}) as docs where doc_id = #{docId}|]
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
          actions <- pgQuery $ actionsList queries actId
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
          VALUES ( #{parent}, #{JsonField (toStructValue newDoc)}
            , #{JsonField (toStructValue act)})
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
    listDocuments = pgQuery (queries ^. field @"documentsList")
