module Tolstoy.DB where

import Data.Aeson
import Data.Pool
import Data.Text as T
import Data.Time
import Database.PostgreSQL.Query as PG
import GHC.Generics (Generic)

type Error = Text

type Action doc act = doc -> act -> Either Error doc

newtype Seqnum = Seqnum
  { unSeqnum :: Integer
  } deriving (Eq, Ord, Show, FromField, ToField, Generic)

data DocDesc doc docId actId = DocDesc
  { doc      :: doc
  , docId    :: docId
  , actionId :: actId
  , seqnum   :: Seqnum
  , created  :: UTCTime
  , modified :: UTCTime
  } deriving (Eq, Ord, Show, Generic)

data Tolstoy doc docId act actId = Tolstoy
  { newDoc
    :: doc
    -> IO (DocDesc doc docId actId)
  -- ^ Inserts a new document in DB
  , getDoc
    :: docId
    -> IO (Maybe (DocDesc doc docId actId))
  -- ^ Get last version of some object
  , changeDoc
    :: DocDesc doc docId actId
    -> act
    -> IO (Either Error (DocDesc doc docId actId))
  -- ^ Saves changed doc to the DB. Note that it does not check the
  -- document history consistency right now.
  } deriving (Generic)

data TolstoyInit doc docId act actId = TolstoyInit
  { exec           :: Action doc docId
  , dbPool         :: Pool PG.Connection
  , documentsTable :: FN
  , actionsTable   :: FN
  } deriving (Generic)

runPG
  :: TolstoyInit doc docId act actId
  -> (HasCallStack => PgMonadT IO a)
  -> IO a
runPG = error "FIXME: runPG not implemented"

tolstoy
  :: forall doc docId act actId
  .  (TolstoyInit doc docId act actId)
  -> Tolstoy act doc docId actId
tolstoy init = Tolstoy { newDoc, getDoc, changeDoc }
  where
    docs = documentsTable
    newDoc = runPG init $ do
      pgQuery [sqlExp|insert into ^{docs} returning id, created_at|]
