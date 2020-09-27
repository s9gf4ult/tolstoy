module Tolstoy.Types.Top where

import Control.Exception
import Data.Aeson
import Data.List.NonEmpty as NE
import Data.Text as T
import Data.Typeable
import Data.UUID.Types
import GHC.Generics (Generic)
import Tolstoy.Types.DB

type TolstoyResult = Either TolstoyError

data TolstoyError
  = AesonError TypeRep String
  | NoMoreVersions TypeRep Integer
  | VersionOutOfBounds Integer
  | ActionNotFound UUID
  | UserActionError Text        --  FIXME: Custom user error type
  | DatabaseAssertionFailed (Maybe TypeRep) Text
  | DatabaseIsNewerThanApp [VersionRep]
  | DatabaseHasIncompatibleMigration VersionRep VersionRep
  | MultipleMigrations (NonEmpty VersionInsert)
  deriving (Eq, Show, Generic)

instance Exception TolstoyError

data InitResult m doc act a
  = InsertBeforeOperation (NonEmpty VersionInsert)
  | Ready (Tolstoy m doc act a)

data VersionRep = VersionRep
  { version  :: !Integer
  , repValue :: !Value
  } deriving (Eq, Show, Generic)

data Tolstoy m doc act a = Tolstoy
  { newDoc
    :: doc
    -- ^ Initial state of the doc.
    -> act
    -- ^ Initial action. It will not be performed on given doc, only
    -- written to DB
    -> m (TolstoyResult (DocDesc doc act))
  -- ^ Inserts a new document in DB
  , getDoc
    :: DocId doc
    -> m (Maybe (TolstoyResult (DocDesc doc act)))
  -- ^ Get last version of some object
  , getDocHistory
    :: DocId doc
    -> m (Maybe (TolstoyResult (DocHistory doc act)))
  -- ^ Get full history of the document
  , changeDoc
    :: DocDesc doc act
    -> act
    -> m (TolstoyResult ((DocDesc doc act), a))
  -- ^ Saves changed doc to the DB. Note that it does not check the
  -- document history consistency from the business logic perspective
  , listDocuments :: m (TolstoyResult [DocDesc doc act])
  } deriving (Generic)
