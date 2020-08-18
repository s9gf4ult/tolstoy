{-# OPTIONS_GHC -Wno-orphans #-}

module Example where

import Control.Lens
import Control.Monad
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

newtype Name = Name
  { unName :: Text
  } deriving (Eq, Ord, Show, Generic, Structural, IsString)

instance Arbitrary Name where
  arbitrary = Arb.elements ["lupa", "pupa", "pepa"]

newtype Email = Email
  { unEmail :: Text
  } deriving (Eq, Ord, Show, Generic, Structural, IsString)

instance Arbitrary Email where
  arbitrary = Arb.elements $ do
    user <- ["user", "buzer", "muzer"]
    host <- ["@example.com", "@super.mail", "@google.com"]
    return $ Email $ user <> host

data User = User
  { name   :: Maybe Name
  , email  :: Email
  , status :: UserStatus
  } deriving (Eq, Ord, Show, Generic)

instance Structural User

instance Arbitrary User where
  arbitrary = genericArbitrary
  shrink = genericShrink

data UserStatus
  = Registered
  | Confirmed
  | Banned
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary UserStatus where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Structural UserStatus

initUser :: User
initUser = User
  { name   = Nothing
  , email  = "user@email"
  , status = Registered }

data UserAction
  = Init
  | SetName Name
  | SetEmail Email
  | Confirm
  | Ban
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary UserAction where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Structural UserAction

actionMigrations :: Migrations 0 '[ UserAction ]
actionMigrations = LastVersion Proxy Proxy

userMigrations :: Migrations 0 '[ User ]
userMigrations = LastVersion Proxy Proxy

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
    tlst = tolstoy userMigrations actionMigrations $ TolstoyInit
      { docAction = userAction
      , documentsTable = "documents"
      , actionsTable = "actions"
      , versionsTable = "versions"
      , doctypeTypeName = "doctype"
      }
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
  insertDescs <- for users $ \u -> newDoc t u Init
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
  userDesc <- newDoc t (User Nothing "Wow@user.com" Registered) Init
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
      (tlst, pool) <- res
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
