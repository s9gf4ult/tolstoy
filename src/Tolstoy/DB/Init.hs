module Tolstoy.DB.Init where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.Except
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

singleElement
  :: forall a m. (Monad m, Typeable a)
  => m [a]
  -> m (TolstoyResult a)
singleElement ma = ma >>= \case
  [a] -> return $ Right a
  _ -> return $ Left $ DatabaseAssertionFailed (Just $ typeRep (Proxy @a))
    "Expected to have single element selected"

optionalElement
  :: forall a m. (Monad m, Typeable a)
  => m [a]
  -> m (Maybe (TolstoyResult a))
optionalElement ma = ma >>= \case
  [] -> return Nothing
  [a] -> return $ Just $ Right a
  _ -> return $ Just $ Left $ DatabaseAssertionFailed
    (Just $ typeRep (Proxy @a))
    "Expected 0 or 1 elements but got more"

tolstoy
  :: forall m doc act a n1 n2 docs acts
  .  ( MonadPostgres m
     , StructuralJSON doc, StructuralJSON act
     , HasCallStack
     , Typeable doc, Typeable act
     , doc ~ Last docs
     , act ~ Last acts
     )
  => Migrations n1 docs
  -> Migrations n2 acts
  -> TolstoyInit doc act a
  -> (Tolstoy m doc act a, TolstoyQueries doc act)
tolstoy docMigrations actMigrations init@TolstoyInit{..} =
  ( Tolstoy { newDoc, getDoc, getDocHistory, changeDoc, listDocuments }
  , queries )
  where
    docLast = lastVersion docMigrations
    actLast = lastVersion actMigrations
    queries = initQueries init
    newDoc document action = runExceptT $ do
      (actionId, modified) <- ExceptT $ singleElement $ pgQuery [sqlExp|
        INSERT INTO ^{actionsTable}
          (document, document_version, action, action_version)
        VALUES
          ( #{JsonField (toStructValue document)}, #{docLast}
          , #{JsonField (toStructValue action)}, #{actLast} )
        RETURNING id, created_at|]
      (documentId, created) <- ExceptT $ singleElement $ pgQuery [sqlExp|
        INSERT INTO ^{documentsTable} (action_id)
        VALUES ( #{actionId} )
        RETURNING id, created_at|]
      let
        res = DocDesc
          { document
          , documentId
          , documentVersion = docLast
          , action
          , actionId
          , actionVersion = actLast
          , created
          , modified }
      return res
    getDoc documentId = do
      res <- optionalElement $ pgQuery [sqlExp|
        SELECT * FROM (^{documentsList queries}) AS docs
        WHERE document_id = #{documentId}|]
      return $ res <&> \descRes ->
        descRes >>= migrateDocDesc docMigrations actMigrations
    getDocHistory documentId = do
      res <- pgQuery [sqlExp|
        SELECT created_at, action_id
        FROM ^{documentsTable}
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
        _ -> return $ Just $ Left $ DatabaseAssertionFailed Nothing
          "Got multiple domutents by id"
    changeDoc docDesc action = case docAction (docDesc ^. field @"document") action of
      Left e       -> return $ Left $ UserActionError e
      Right (newDoc, a) -> runExceptT $ do
        let
          parent = docDesc ^. field @"actionId"
          docId = docDesc ^. field @"documentId"
        (newActId, newMod) <- ExceptT $ singleElement $ pgQuery [sqlExp|
          INSERT INTO ^{actionsTable}
            (parent_id, document, document_version, action, action_version)
          VALUES
            ( #{parent}
            , #{JsonField (toStructValue newDoc)}, #{docLast}
            , #{JsonField (toStructValue action)}, #{actLast} )
          RETURNING id, created_at|]
        check <- pgExecute [sqlExp|UPDATE ^{documentsTable}
          SET action_id = #{newActId}
          WHERE id = #{docId}|]
        unless (check == 1) $ throwError $ DatabaseAssertionFailed Nothing
          "Document was not updated"
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
        return (res, a)
    listDocuments = do
      raw <- pgQuery $ documentsList queries
      return $ traverse (migrateDocDesc docMigrations actMigrations) raw
