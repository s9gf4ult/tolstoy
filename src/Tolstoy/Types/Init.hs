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
  { documentsList  :: SqlBuilder
  -- ^ List of latest versions of documents. Response must be parsed
  -- as DocumentsListRaw
  , selectDocument :: DocId doc -> SqlBuilder
  -- ^ Selects single document. Response must be parsed as
  -- DocumentsListRaw
  , actionsList    :: ActId act -> SqlBuilder
  -- ^ Get action id to get started from
  , selectVersions :: SqlBuilder
  , insertVersions :: NonEmpty VersionInsert -> SqlBuilder
  , insertAction   :: InsertAction doc act -> SqlBuilder
  , insertDocument :: ActId act -> SqlBuilder
  , setActionId    :: DocId doc -> ActId act -> SqlBuilder
  } deriving (Generic)
