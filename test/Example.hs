module Example where

import Control.Lens
import Control.Monad
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Data.Aeson
import Data.Generics.Product
import Data.List.NonEmpty as NE
import Data.Pool as Pool
import Data.Text as T
import Database.PostgreSQL.Query as PG
import Database.PostgreSQL.Simple as PG
import GHC.Generics (Generic)
import Test.Tasty as Test
import Test.Tasty.HUnit as Test
import Tolstoy.DB

data User = User
  { name   :: Maybe Text
  , email  :: Text
  , status :: UserStatus
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON User
instance FromJSON User

data UserStatus
  = Registered
  | Confirmed
  | Banned
  deriving (Eq, Ord, Show, Generic)

instance ToJSON UserStatus
instance FromJSON UserStatus

initUser :: User
initUser = User
  { name   = Nothing
  , email  = "user@email"
  , status = Registered }

data UserAction
  = Init
  | SetName Text
  | SetEmail Text
  | Confirm
  | Ban
  deriving (Eq, Ord, Show, Generic)

instance ToJSON UserAction
instance FromJSON UserAction

userAction :: PureDocAction User UserAction
userAction = pureDocAction $ \user -> \case
  SetName name -> do
    checkStatus user
    return $ user & field @"name" .~ Just name
  SetEmail e -> do
    checkStatus user
    return $ user & field @"email" .~ e
  Confirm -> do
    checkStatus user
    return $ user & field @"status" .~ Confirmed
  Ban -> return $ user & field @"status" .~ Banned
  where
    checkStatus user = case status user of
      Banned -> Left "User is banned"
      _      -> pure ()

runTest :: Pool Connection -> TestMonad a -> IO a
runTest p t = do
  runStderrLoggingT $ Pool.withResource p $ \con -> runPgMonadT con t

closeDB :: (Testoy, Pool Connection) -> IO ()
closeDB (tlst, p) = runTest p $ do
  void $ pgExecute $ tlst ^. field @"queries" . field @"revert"

-- | All parameters will be taken from env variables by @libpq@
openDB :: IO (Testoy, Pool Connection)
openDB = do
  p <- createPool (PG.connectPostgreSQL "") PG.close 1 1 1
  let
    tlst :: Testoy
    tlst = tolstoy $ TolstoyInit
      { docAction = userAction
      , documentsTable = "documents"
      , actionsTable = "actions" }
  void $ runTest p $ do
    pgExecute $ tlst ^. field @"queries" . field @"deploy"
  return (tlst, p)

type TestMonad = PgMonadT (LoggingT IO)

instance MonadFail TestMonad where
  fail = error

type Testoy = Tolstoy TestMonad User UserAction ()

createAndRead
  :: Tolstoy TestMonad User UserAction ()
  -> TestMonad ()
createAndRead t = do
  let user = User Nothing "wow@such.email" Registered
  desc <- newDoc t user Init
  let did = desc ^. field @"docId"
  newDesc <- getDoc t did
  liftIO $ assertEqual "getDoc" (Just desc) newDesc
  Just hist <- getDocHistory t did
  let story :| [] = hist ^. field @"history"
  liftIO $ assertEqual "getHist.doc" (desc ^. field @"doc") (story ^. field @"doc")
  liftIO $ assertEqual "getHist.act" (desc ^. field @"act") (story ^. field @"act")
  liftIO $ assertEqual "getHist.actId" (desc ^. field @"actId") (story ^. field @"actId")


test_UserActions :: TestTree
test_UserActions = Test.withResource openDB closeDB $ \res ->
  let
    exec :: TestName -> (Testoy -> TestMonad ()) -> TestTree
    exec n ma = testCase n $ do
      (tlst, pool) <- res
      runTest pool $ ma tlst
  in testGroup "UserActions"
     [ exec "Create and read" createAndRead ]
