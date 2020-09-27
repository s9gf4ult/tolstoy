module Tolstoy.Types.DB where

import Control.Monad.Except
import Data.Aeson
import Data.Char as C
import Data.List.NonEmpty as NE
import Data.Time
import Data.Typeable
import Data.UUID.Types
import Database.PostgreSQL.Query as PG
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics (Generic)
import Prelude as P
import Tolstoy.Structure


newtype Seqnum = Seqnum
  { unSeqnum :: Integer
  } deriving (Eq, Ord, Show, FromField, ToField, Generic)

newtype DocId doc = DocId
  { unDocId :: UUID
  } deriving (Eq, Ord, Show, Generic, FromField, ToField, FromJSON, ToJSON)

newtype ActId act = ActId
  { unActId :: UUID
  } deriving (Eq, Ord, Show, Generic, FromField, ToField, FromJSON, ToJSON)

newtype JsonField a = JsonField
  { unJsonField :: a
  } deriving (Eq, Ord, Show, Generic)

instance (FromJSON a, Typeable a) => FromField (JsonField a) where
  fromField a b = JsonField <$> fromJSONField a b

instance (ToJSON a) => ToField (JsonField a) where
  toField = toJSONField . unJsonField

type StructuralJSON doc =
  ( Structural doc
  , Typeable (StructureValue (StructKind doc))
  , FromJSON (StructureValue (StructKind doc))
  , ToJSON (StructureValue (StructKind doc)) )

data Doctype = Document | Action
  deriving (Eq, Ord, Show, Generic)

derivePgEnum (fmap C.toLower) ''Doctype

data VersionRaw = VersionRaw
  { id            :: !UUID
  , doctype       :: !Doctype
  , version       :: !Integer
  , structure_rep :: !Value
  , created_at    :: !UTCTime
  } deriving (Eq, Show, Generic)

instance FromRow VersionRaw

data VersionInsert = VersionInsert
  { doctype       :: !Doctype
  , version       :: !Integer
  , structure_rep :: !Value
  } deriving (Eq, Show, Generic)

data DocDesc doc act = DocDesc
  { document        :: !doc
  , documentId      :: !(DocId doc)
  , documentVersion :: !Integer
  -- ^ Original version number before migration
  , action          :: !act
  , actionId        :: !(ActId act)
  , actionVersion   :: !Integer
  -- ^ Original version number before migration
  , created         :: !UTCTime
  , modified        :: !UTCTime
  } deriving (Eq, Ord, Show, Generic)

-- | Convenient type to parse DocDesc from sql
data DocDescRaw = DocDescRaw
  { document        :: JsonField Value
  , documentId      :: UUID
  , documentVersion :: Integer
  , action          :: JsonField Value
  , actionId        :: UUID
  , actionVersion   :: Integer
  , created         :: UTCTime
  , modified        :: UTCTime
  } deriving (Eq, Show, Generic)

instance FromRow DocDescRaw

data DocHistory doc act = DocHistory
  { documentId :: !(DocId doc)
  , created    :: !UTCTime
  , history    :: !(NonEmpty (Story doc act))
  -- ^ Story points in reverse order. Head is the last actual version
  -- and tail is the initial
  } deriving (Eq, Ord, Show, Generic)

data Story doc act = Story
  { document        :: !doc
  , documentVersion :: !Integer
  -- ^ Original document version number before migration
  , action          :: !act
  , actionId        :: !(ActId act)
  , actionVersion   :: !Integer
  -- ^ Original action version number before migration
  , modified        :: !UTCTime
  , parentId        :: !(Maybe (ActId act))
  } deriving (Eq, Ord, Show, Generic)

data ActionRaw = ActionRaw
  { document        :: JsonField Value
  , documentVersion :: Integer
  , action          :: JsonField Value
  , actionId        :: UUID
  , actionVersion   :: Integer
  , modified        :: UTCTime
  , parentId        :: Maybe UUID
  } deriving (Eq, Show, Generic)

instance FromRow ActionRaw
