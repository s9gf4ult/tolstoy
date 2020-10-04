module UsersExample.MigrationTest where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Data.Generics.Product
import           Data.Pool as Pool
import           Database.PostgreSQL.Query as PG
import           Database.PostgreSQL.Simple as PG
import           Test.Tasty as Test
import           Test.Tasty.HUnit as Test
import           Test.Tasty.QuickCheck
import           Tolstoy.DB
import           Tolstoy.Migration
import           Tolstoy.Structure
import           Tolstoy.Types
import qualified UsersExample.Data.V0 as V0
import qualified UsersExample.Data.V1 as V1
import           UsersExample.Shared

openDB :: IO (Pool Connection)
openDB = createPool (PG.connectPostgreSQL "") PG.close 1 1 1

closeDB :: Pool Connection -> IO ()
closeDB _ = return ()

mig0ThenMig1 :: IO (Pool Connection) -> IO ()
mig0ThenMig1 getPool = do
  p <- getPool
  runTest p $ do
    let
      tinit = TolstoyInit
        { docAction = V0.userAction
        , documentsTable = "documents"
        , actionsTable = "actions"
        , versionsTable = "versions"
        , doctypeTypeName = "doctype" }
      tinit1 = tinit { docAction = V1.userAction}
      queries = initQueries tinit
      queries1 = initQueries tinit1
    void $ pgExecute $ queries ^. field @"deploy"
    tlst0 :: Tolstoy TestMonad V0.User V0.UserAction () <-
      autoDeploy (versionsTable tinit)
      (tolstoyInit V0.userMigrations V0.actionMigrations tinit queries) >>=
      either throwM return
    tlst1 :: Tolstoy TestMonad V1.User V1.UserAction () <-
      autoDeploy (versionsTable tinit1)
      (tolstoyInit V1.userMigrations V1.actionMigrations tinit1 queries1) >>=
      either throwM return
    void $ pgExecute $ queries ^. field @"revert"


test_Migrations :: TestTree
test_Migrations = Test.withResource openDB closeDB $ \getPool ->
  testGroup "Migrations example"
  [ testCase "Run migration 0 then successfuly run 1" $ mig0ThenMig1 getPool ]
