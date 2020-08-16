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
import           Prelude as P
import           Tolstoy.DB.Types
import           Tolstoy.Migration
import           Tolstoy.Structure

initQueries :: TolstoyInit doc act a -> TolstoyQueries doc act
initQueries init = TolstoyQueries { deploy, revert, documentsList, actionsList }
  where
    documents = documentsTable init
    actions = actionsTable init
    versions = versionsTable init
    doctypeName = doctypeTypeName init
    documentsList = $(sqlExpFile "documentsList")
    deploy = $(sqlExpFile "deploy")
    revert = $(sqlExpFile "revert")
    actionsList actionId = $(sqlExpFile "actionsList")

tolstoy
  :: forall m doc act a n1 n2 docs acts
  .  ( MonadPostgres m
     , MonadFail m              -- FIXME: KILL it
     , StructuralJSON doc, StructuralJSON act
     , HasCallStack
     , doc ~ Last docs
     , act ~ Last acts
     )
  => Migrations n1 docs
  -> Migrations n2 acts
  -> TolstoyInit doc act a
  -> Tolstoy m doc act a
tolstoy docMigrations actMigrations init =
  Tolstoy { newDoc, getDoc, getDocHistory, changeDoc, listDocuments, queries }
  where
    docLast = lastVersion docMigrations
    actLast = lastVersion actMigrations
    queries = initQueries init
    newDoc document action = do
      [(actionId, modified)] <- pgQuery [sqlExp|
        INSERT INTO ^{actionsTable init}
          (document, document_version, action, action_version)
        VALUES
          ( #{JsonField (toStructValue document)}, #{docLast}
          , #{JsonField (toStructValue action)}, #{actLast} )
        RETURNING id, created_at|]
      [(documentId, created)] <- pgQuery [sqlExp|
        INSERT INTO ^{documentsTable init} (action_id)
        VALUES ( #{actionId} )
        RETURNING id, created_at|]
      let
        res = DocDesc
          { document , documentId , action , actionId , created, modified }
      return res
    getDoc documentId = do
      res <- pgQuery [sqlExp|
        SELECT * FROM (^{documentsList queries}) AS docs
        WHERE document_id = #{documentId}|]
      case res of
        []        -> return Nothing
        [docDesc] -> return $ Just
          $ migrateDocDesc docMigrations actMigrations docDesc
        _         -> error "Unexpected count of results"
    getDocHistory documentId = do
      res <- pgQuery [sqlExp|
        SELECT created_at, action_id
        FROM ^{documentsTable init}
        WHERE id = #{documentId}|]
      case res of
        [] -> return Nothing
        [(created, actionId)] -> do
          actions <- pgQuery $ actionsList queries actionId
          case actionsHistory docMigrations actMigrations actionId actions of
            Left err    -> return $ Just $ Left err
            Right slist -> case NE.nonEmpty slist of
              Nothing      -> return $ Just $ Left
                $ ActionNotFound $ unActId actionId
              Just history -> return $ Just $ Right
                $ DocHistory {documentId, created, history}
    changeDoc docDesc action = case docAction init (docDesc ^. field @"document") action of
      Left e       -> return $ Left e
      Right (newDoc, a) -> do
        let
          parent = docDesc ^. field @"actionId"
          docId = docDesc ^. field @"documentId"
        [(newActId, newMod)] <- pgQuery [sqlExp|
          INSERT INTO ^{actionsTable init}
            (parent_id, document, document_version, action, action_version)
          VALUES
            ( #{parent}
            , #{JsonField (toStructValue newDoc)}, #{docLast}
            , #{JsonField (toStructValue action)}, #{actLast} )
          RETURNING id, created_at|]
        1 <- pgExecute [sqlExp|UPDATE ^{documentsTable init}
          SET action_id = #{newActId}
          WHERE id = #{docId}|]
        let
          res =  DocDesc
            { document        = newDoc
            , documentId      = docId
            , documentVersion = docLast
            , action          = action
            , actionId        = newActId
            , actionVersion   = actLast
            , created         = docDesc ^. field @"created"
            , modified        = newMod }
        return $ Right (res, a)
    listDocuments = do
      raw <- pgQuery $ getField @"documentsList" queries
      return $ traverse (migrateDocDesc docMigrations actMigrations) raw
