{-# OPTIONS_GHC -Wno-orphans #-}

module Example where

import Control.Lens
import Control.Monad
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson
import Data.Generics.Product
import Data.List.NonEmpty as NE
import Data.Pool as Pool
import Data.Set as S
import Data.Text as T
import Data.Traversable
import Database.PostgreSQL.Query as PG
import Database.PostgreSQL.Simple as PG
import GHC.Generics (Generic)
import Prelude as P
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
  Init         -> return user
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

createAndRead :: Testoy -> TestMonad ()
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

listAndChange :: Testoy -> TestMonad ()
listAndChange t = do
  let
    users = ["user1", "user2", "user3"] <&> \e ->
      User Nothing e Registered
  insertDescs <- for users $ \u -> newDoc t u Init
  gotDescs <- listDocuments t
  let
    s1 = S.fromList insertDescs
    s2 = S.fromList gotDescs
  liftIO $ assertEqual "intersect 3" 3 (S.size $ S.intersection s1 s2)
  void $ changeDoc t (P.head insertDescs) Ban
  newDocs <- listDocuments t
  liftIO $ assertEqual "intersect 2" 2
    (S.size $ S.intersection s1 $ S.fromList newDocs)

changeSingleDoc :: Testoy -> TestMonad ()
changeSingleDoc t = do
  userDesc <- newDoc t (User Nothing "Wow@user.com" Registered) Init
  let n = "Lupa"
  Right (named, ()) <- changeDoc t userDesc $ SetName n
  liftIO $ assertEqual "Name set" (Just n)
    $ named ^. field @"doc" . field @"name"
  Right (confirmed, ()) <- changeDoc t named Confirm
  liftIO $ assertEqual "Status confirmed" Confirmed
    $ confirmed ^. field @"doc" . field @"status"

test_UserActions :: TestTree
test_UserActions = Test.withResource openDB closeDB $ \res ->
  let
    exec :: TestName -> (Testoy -> TestMonad ()) -> TestTree
    exec n ma = testCase n $ do
      (tlst, pool) <- res
      runTest pool $ ma tlst
  in testGroup "UserActions"
     [ exec "Create and read" createAndRead
     , exec "List several documents and see list changes" listAndChange
     , exec "Change single doc and see it changes" changeSingleDoc
     ]
