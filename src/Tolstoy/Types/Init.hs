module Tolstoy.Types.Init where

import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text as T
import qualified Data.Text.Lazy as TL
import           Database.PostgreSQL.Query
import           GHC.Generics (Generic)
import           Tolstoy.Structure
import           Tolstoy.Types.DB

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

data ListDocuments doc ret
  = ListAll
  | ByCondition (StructureCondition (StructKind doc) 'Nothing)
  -- ^ List documents by condition
  | ByQuery (StructureQuery (StructKind doc) 'Nothing ret)
  -- ^ List documents by non-empty query

data TolstoyQueries doc act = TolstoyQueries
  { documentsList  :: forall ret. ListDocuments doc ret -> SqlBuilder
  -- ^ List of latest versions of documents. Response must be parsed
  -- as DocumentsListRaw
  , subDocumentsList
    :: forall ret inner
    .  ListDocuments doc inner
    -> StructureQuery (StructKind doc) 'Nothing (StructKind ret)
    -> SqlBuilder
  -- ^ List document parts selected by queries
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
  }
