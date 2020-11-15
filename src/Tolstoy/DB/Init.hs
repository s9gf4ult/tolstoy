module Tolstoy.DB.Init where

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Except
import           Data.Generics.Product
import           Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Semigroup as Sem
import           Data.Typeable
import           Data.UUID.Types
import           Database.PostgreSQL.Query as PG
import           GHC.Stack
import           Prelude as P
import           Tolstoy.Migration
import           Tolstoy.Structure
import           Tolstoy.Types

initQueries
  :: (Structural doc, Structural act)
  => TolstoyTables
  -> TolstoyQueries doc act
initQueries TolstoyTables{..} = TolstoyQueries
  { deploy = $(sqlExpFile "deploy")
  , revert = $(sqlExpFile "revert")
  , documentsList = $(sqlExpFile "documentsList")
  , actionsList = \actionId -> $(sqlExpFile "actionsList")
  , selectVersions = $(sqlExpFile "selectVersions")
  , insertVersions
  , insertAction
  }
  where
    insertVersions vs = [sqlExp|
      INSERT INTO ^{versionsTable} (doctype, "version", structure_rep)
      VALUES ^{values}|]
      where
        values = Sem.sconcat $ NE.intersperse ", " $ toRow <$> vs
        toRow (VersionInsert d v s) = [sqlExp|(#{d}, #{v}, #{s})|]
    insertAction InsertAction{..} = $(sqlExpFile "insertAction")

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

dbToVersionRep :: VersionRaw -> VersionRep
dbToVersionRep r = VersionRep
  { version = r ^. field @"version"
  , repValue = r ^. field @"structure_rep" }

versionToInsert :: Doctype -> VersionRep -> VersionInsert
versionToInsert doctype r = VersionInsert
  { doctype
  , version = r ^. field @"version"
  , structure_rep = r ^. field @"repValue" }

autoDeploy
  :: (MonadPostgres n)
  => TolstoyQueries doc act
  -> n (TolstoyResult (InitResult m doc act a))
  -> n (TolstoyResult (Tolstoy m doc act a))
autoDeploy queries init = runExceptT $ do
  res <- ExceptT init
  case res of
    InsertBeforeOperation ins -> do
      lift $ pgExecute $ insertVersions queries ins
      again <- ExceptT init
      case again of
        InsertBeforeOperation wut -> do
          throwError $ MultipleMigrations wut
        Ready t -> return t
    Ready t -> return t

migrateDocDesc
  :: MigMap doc
  -> MigMap act
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

actionsHistory
  :: forall doc act
  .  MigMap doc
  -> MigMap act
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


tolstoyAutoInit
  :: forall m n doc act a n1 n2 docs acts
  .  ( MonadPostgres m
     , MonadPostgres n, MonadThrow n
     , StructuralJSON doc, StructuralJSON act
     , HasCallStack
     , Typeable doc, Typeable act
     , doc ~ Head docs
     , act ~ Head acts
     )
  => Migrations n1 docs
  -> Migrations n2 acts
  -> DocAction doc act a
  -> TolstoyTables
  -> n (Tolstoy m doc act a)
tolstoyAutoInit docMigrations actMigrations docAction tables = do
  autoDeploy queries
    (tolstoyInit docMigrations actMigrations docAction tables queries)
    >>= either throwM return
  where
    queries = initQueries tables

tolstoyInit
  :: forall m n doc act a n1 n2 docs acts
  .  ( MonadPostgres m
     , MonadPostgres n
     , StructuralJSON doc, StructuralJSON act
     , HasCallStack
     , Typeable doc, Typeable act
     , doc ~ Head docs
     , act ~ Head acts
     )
  => Migrations n1 docs
  -> Migrations n2 acts
  -> DocAction doc act a
  -> TolstoyTables
  -> TolstoyQueries doc act
  -> n (TolstoyResult (InitResult m doc act a))
tolstoyInit docMigrations actMigrations docAction init@TolstoyTables{..} queries = do
  dbVersions <- pgQuery $ selectVersions queries
  let
    dbDocVersions = dbVersions ^.. traversed
      . filtered (views (field @"doctype") (== Document))
      . to dbToVersionRep
    dbActVersions = dbVersions ^.. traversed
      . filtered (views (field @"doctype") (== Action))
      . to dbToVersionRep
    checkResult = do
      NeedsDeploy docReps <- checkVersions docMigrations dbDocVersions
      NeedsDeploy actReps <- checkVersions actMigrations dbActVersions
      let
        inserts = (versionToInsert Document <$> docReps)
          ++ (versionToInsert Action <$> actReps)
      return $ case NE.nonEmpty inserts of
        Nothing -> Ready
          Tolstoy { newDoc, getDoc, getDocHistory, changeDoc, listDocuments }
        Just ine -> InsertBeforeOperation ine
  return checkResult
  where
    docMigMap = migMap docMigrations
    actMigMap = migMap actMigrations
    docIndex = actualMigrationIndex docMigrations
    actIndex = actualMigrationIndex actMigrations
    newDoc document action = runExceptT $ do
      (actionId, modified) <- ExceptT $ singleElement $ pgQuery
        $ insertAction queries $ InsertAction
        { document
        , documentVersion = docIndex
        , action
        , actionVersion = actIndex }
      (documentId, created) <- ExceptT $ singleElement $ pgQuery [sqlExp|
        INSERT INTO ^{documentsTable} (action_id)
        VALUES ( #{actionId} )
        RETURNING id, created_at|]
      let
        res = DocDesc
          { document
          , documentId
          , documentVersion = docIndex
          , action
          , actionId
          , actionVersion = actIndex
          , created
          , modified }
      return res
    getDoc documentId = do
      res <- optionalElement $ pgQuery [sqlExp|
        SELECT * FROM (^{documentsList queries}) AS docs
        WHERE document_id = #{documentId}|]
      return $ res <&> \descRes ->
        descRes >>= migrateDocDesc docMigMap actMigMap
    getDocHistory documentId = do
      res <- pgQuery [sqlExp|
        SELECT created_at, action_id
        FROM ^{documentsTable}
        WHERE id = #{documentId}|]
      case res of
        [] -> return Nothing
        [(created, actionId)] -> do
          actions <- pgQuery $ actionsList queries actionId
          case actionsHistory docMigMap actMigMap actionId actions of
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
            , #{JsonField (toStructValue newDoc)}, #{docIndex}
            , #{JsonField (toStructValue action)}, #{actIndex} )
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
            , documentVersion = docIndex
            , action          = action
            , actionId        = newActId
            , actionVersion   = actIndex
            , created         = docDesc ^. field @"created"
            , modified        = newMod }
        return (res, a)
    listDocuments = do
      raw <- pgQuery $ documentsList queries
      return $ traverse (migrateDocDesc docMigMap actMigMap) raw
