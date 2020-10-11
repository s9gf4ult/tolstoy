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

initQueries :: TolstoyTables -> TolstoyQueries doc act
initQueries init = TolstoyQueries { deploy, revert, documentsList, actionsList }
  where
    documents = documentsTable  init
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

dbToVersionRep :: VersionRaw -> VersionRep
dbToVersionRep r = VersionRep
  { version = r ^. field @"version"
  , repValue = r ^. field @"structure_rep" }

versionToInsert :: Doctype -> VersionRep -> VersionInsert
versionToInsert doctype r = VersionInsert
  { doctype
  , version = r ^. field @"version"
  , structure_rep = r ^. field @"repValue" }

insertVersions
  :: (MonadPostgres m)
  => FN
  -> NonEmpty VersionInsert
  -> m ()
insertVersions versions vs = void $ pgExecute [sqlExp|
  INSERT INTO ^{versions} (doctype, "version", structure_rep)
  VALUES ^{values}|]
  where
    values = Sem.sconcat $ NE.intersperse ", " $ toRow <$> vs
    toRow (VersionInsert d v s) = [sqlExp|(#{d}, #{v}, #{s})|]

autoDeploy
  :: (MonadPostgres n, MonadThrow m)
  => FN
  -- ^ Versions table
  -> n (TolstoyResult (InitResult m doc act a))
  -> n (TolstoyResult (Tolstoy m doc act a))
autoDeploy versions init = runExceptT $ do
  res <- ExceptT init
  case res of
    InsertBeforeOperation ins -> do
      lift $ insertVersions versions ins
      again <- ExceptT init
      case again of
        InsertBeforeOperation wut -> do
          throwError $ MultipleMigrations wut
        Ready t -> return t
    Ready t -> return t

migrateDocDesc
  :: forall n1 docs n2 acts doc act
  .  (doc ~ Head docs, act ~ Head acts)
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

actionsHistory
  :: forall n1 docs n2 acts doc act
  . (act ~ Head acts, doc ~ Head docs)
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
  dbVersions <- pgQuery [sqlExp|SELECT
    id,
    doctype,
    version,
    structure_rep,
    created_at
    FROM ^{versionsTable}
    ORDER BY version ASC|]
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
    docIndex = actualMigrationIndex docMigrations
    actIndex = actualMigrationIndex actMigrations
    newDoc document action = runExceptT $ do
      (actionId, modified) <- ExceptT $ singleElement $ pgQuery [sqlExp|
        INSERT INTO ^{actionsTable}
          (document, document_version, action, action_version)
        VALUES
          ( #{JsonField (toStructValue document)}, #{docIndex}
          , #{JsonField (toStructValue action)}, #{actIndex} )
        RETURNING id, created_at|]
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
      return $ traverse (migrateDocDesc docMigrations actMigrations) raw
