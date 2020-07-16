module Example where

import Control.Lens
import Control.Monad
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Data.Aeson
import Data.Generics.Product
import Data.Pool as Pool
import Data.Text as T
import Database.PostgreSQL.Query as PG
import Database.PostgreSQL.Simple as PG
import GHC.Generics (Generic)
-- import Test.HUnit
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

-- | All parameters will be taken from env variables by @libpq@
openDB :: IO (Pool Connection)
openDB = createPool (PG.connectPostgreSQL "") PG.close 1 1 1

runTest :: IO (Pool Connection) -> TestMonad a -> IO a
runTest p' t = do
  p <- p'
  runStderrLoggingT $ Pool.withResource p $ \con -> runPgMonadT con t

closeDB :: Pool Connection -> IO ()
closeDB p = runTest (pure p) $ do
  void $ pgExecute [sqlExp|drop table documents|]
  void $ pgExecute [sqlExp|drop table actions|]


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
  newDesc <- getDoc t (desc ^. field @"docId")
  liftIO $ assertEqual "Fail" (Just desc) newDesc

test_UserActions :: TestTree
test_UserActions = Test.withResource openDB (const $ return ()) $ \pool ->
  let
    tlst :: Testoy
    tlst = tolstoy $ TolstoyInit
      { docAction = userAction
      , documentsTable = "documents"
      , actionsTable = "actions"
      }
    exec :: TestName -> (Testoy -> TestMonad ()) -> TestTree
    exec n ma = testCase n $ runTest pool $ ma tlst
  in testGroup "UserActions"
     [ exec "Create and read" createAndRead ]
