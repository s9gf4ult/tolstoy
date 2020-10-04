{-# OPTIONS_GHC -Wno-orphans #-}

module UsersExample.CrudTest where

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Generics.Product
import Data.List.NonEmpty as NE
import Data.Pool as Pool
import Data.Proxy
import Data.Set as S
import Data.String
import Data.Text as T
import Data.Traversable
import Database.PostgreSQL.Query as PG
import Database.PostgreSQL.Simple as PG
import GHC.Generics (Generic)
import Prelude as P
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen as Arb
import Test.Tasty as Test
import Test.Tasty.HUnit as Test
import Test.Tasty.QuickCheck
import Tolstoy.DB
import Tolstoy.Migration
import Tolstoy.Structure
import Tolstoy.Types
import UsersExample.Data.V0


runTest :: Pool Connection -> TestMonad a -> IO a
runTest p t = do
  runStderrLoggingT $ Pool.withResource p $ \con -> runPgMonadT con t

closeDB :: (Testoy, TestoyQ, Pool Connection) -> IO ()
closeDB (_tlst, queries, p) = runTest p $ do
  void $ pgExecute $ queries ^. field @"revert"

-- | All parameters will be taken from env variables by @libpq@
openDB :: IO (Testoy, TestoyQ, Pool Connection)
openDB = do
  p <- createPool (PG.connectPostgreSQL "") PG.close 1 1 1
  let
    tinit = TolstoyInit
      { docAction = userAction
      , documentsTable = "documents"
      , actionsTable = "actions"
      , versionsTable = "versions"
      , doctypeTypeName = "doctype"
      }
    queries = initQueries tinit
    orDrop ma = onException ma $ do
      void $ pgExecute $ queries ^. field @"revert"
  tlst <- runTest p $ orDrop $ do
    void $ pgExecute $ queries ^. field @"deploy"
    autoDeploy (versionsTable tinit)
      (tolstoyInit userMigrations actionMigrations tinit queries)
      >>= either throwM return
  return (tlst, queries, p)

type TestMonad = PgMonadT (LoggingT IO)

instance MonadFail TestMonad where
  fail = error

type Testoy = Tolstoy TestMonad User UserAction ()

type TestoyQ = TolstoyQueries User UserAction

createAndRead :: Testoy -> TestMonad ()
createAndRead t = do
  let user = User Nothing "wow@such.email" Registered
  Right desc <- newDoc t user Init
  let did = desc ^. field @"documentId"
  newDesc <- getDoc t did
  liftIO $ assertEqual "getDoc" (Just $ Right desc) newDesc
  Just (Right hist) <- getDocHistory t did
  let story :| [] = hist ^. field @"history"
  liftIO $ assertEqual "getHist.doc" (desc ^. field @"document") (story ^. field @"document")
  liftIO $ assertEqual "getHist.act" (desc ^. field @"action") (story ^. field @"action")
  liftIO $ assertEqual "getHist.actId" (desc ^. field @"actionId") (story ^. field @"actionId")

listAndChange :: Testoy -> TestMonad ()
listAndChange t = do
  let
    users = ["user1", "user2", "user3"] <&> \e ->
      User Nothing e Registered
  insertDescs <- for users $ \u -> do
    Right desc <- newDoc t u Init
    return desc
  Right gotDescs <- listDocuments t
  let
    s1 = S.fromList insertDescs
    s2 = S.fromList gotDescs
  liftIO $ assertEqual "intersect 3" 3 (S.size $ S.intersection s1 s2)
  void $ changeDoc t (P.head insertDescs) Ban
  Right newDocs <- listDocuments t
  liftIO $ assertEqual "intersect 2" 2
    (S.size $ S.intersection s1 $ S.fromList newDocs)

changeSingleDoc :: Testoy -> TestMonad ()
changeSingleDoc t = do
  Right userDesc <- newDoc t (User Nothing "Wow@user.com" Registered) Init
  let n = "Lupa"
  Right (named, ()) <- changeDoc t userDesc $ SetName n
  liftIO $ assertEqual "Name set" (Just n)
    $ named ^. field @"document" . field @"name"
  Right (confirmed, ()) <- changeDoc t named Confirm
  liftIO $ assertEqual "Status confirmed" Confirmed
    $ confirmed ^. field @"document" . field @"status"

pureTests :: TestTree
pureTests = testGroup "Pure tests"
  [ testProperty "Structural User" $ \(user :: User) ->
      fromStructValue (toStructValue user) == user
  , testProperty "Structural UserAction" $ \(act :: UserAction) ->
      fromStructValue (toStructValue act) == act
  ]

ioTests :: TestTree
ioTests = Test.withResource openDB closeDB $ \res ->
  let
    exec :: TestName -> (Testoy -> TestMonad ()) -> TestTree
    exec n ma = testCase n $ do
      (tlst, _queries, pool) <- res
      runTest pool $ ma tlst
  in testGroup "IO tests"
     [ exec "Create and read" createAndRead
     , exec "List several documents and see list changes" listAndChange
     , exec "Change single doc and see it changes" changeSingleDoc
     ]

test_UserActions :: TestTree
test_UserActions = testGroup "Users example"
  [ pureTests
  , Test.after AllSucceed "Pure tests" ioTests ]
