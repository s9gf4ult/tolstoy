module Tolstoy.Types.Init where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text as T
import Database.PostgreSQL.Query
import GHC.Generics (Generic)
import Tolstoy.Types.DB

type Error = Text

type DocAction doc act a = doc -> act -> Either Error (doc, a)

type PureDocAction doc act = DocAction doc act ()

pureDocAction :: (doc -> act -> Either Error doc) -> PureDocAction doc act
pureDocAction f doc act = (,()) <$> f doc act

data TolstoyTables = TolstoyTables
  { documentsTable  :: !FN
  , actionsTable    :: !FN
  , versionsTable   :: !FN
  , doctypeTypeName :: !FN
  } deriving (Generic)

data TolstoyQueries doc act = TolstoyQueries
  { deploy         :: SqlBuilder
  -- ^ Deploy tables to the database
  , revert         :: SqlBuilder
  -- ^ Revert tables (drop em)
  , documentsList  :: SqlBuilder
  -- ^ List of latest versions of documents
  , actionsList    :: ActId act -> SqlBuilder
  -- ^ Get action id to get started from
  , selectVersions :: SqlBuilder
  , insertVersions :: NonEmpty VersionInsert -> SqlBuilder
  , insertAction   :: InsertAction doc act -> SqlBuilder
  , insertDocument :: ActId act -> SqlBuilder
  } deriving (Generic)
