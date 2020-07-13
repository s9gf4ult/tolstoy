module Tolstoy.Migration where

import           Data.Maybe
import           Data.Proxy
import           Data.Scientific
import           Data.Text as T
import qualified Data.Vector as V
import           GHC.TypeLits
import qualified Prelude as P

-- | Kind for describing the structure of the document
data Structure
  = String
  | Number
  | Bool
  | Null
  | Optional Structure
  | Vector Structure
  | Sum [(Symbol, Structure)]
  | Product [(Symbol, Structure)]

data StructureRep :: Structure -> * where
  StringRep   :: StructureRep String
  NumberRep   :: StructureRep Number
  BoolRep     :: StructureRep Bool
  NullRep     :: StructureRep Null
  OptionalRep :: StructureRep s -> StructureRep (Optional s)
  VectorRep   :: StructureRep s -> StructureRep (Vector s)
  SumRep      :: TaggedListRep l -> StructureRep (Sum l)
  ProductRep  :: TaggedListRep l -> StructureRep (Product l)

data TaggedListRep :: [(Symbol, Structure)] -> * where
  TaggedListNil :: TaggedListRep '[]
  TaggedListCons
    :: (KnownSymbol t)
    => Proxy t
    -> StructureRep s
    -> TaggedListRep tail
    -> TaggedListRep ('(t, s) ': tail)

-- | Materialize any structure type to it's representation
class KnownStructure (s :: Structure) where
  structureRep :: StructureRep s

data StructureValue :: Structure -> * where
  StringValue   :: Text -> StructureValue String
  NumberValue   :: Scientific -> StructureValue Number
  BoolValue     :: P.Bool -> StructureValue Bool
  NullValue     :: StructureValue Null
  OptionalValue :: Maybe (StructureValue s) -> StructureValue (Optional s)
  VectorValue   :: V.Vector (StructureValue s) -> StructureValue (Vector s)
  SumValue      :: SumValueL l -> StructureValue (Sum l)
  ProductValue  :: ProductValueL l -> StructureValue (Product l)

data SumValueL :: [(Symbol, Structure)] -> * where
  ThisValue
    :: forall t s tail
    .  (KnownSymbol t)
    => Proxy t
    -> StructureValue s
    -> SumValueL ('(t, s) ': tail)
  ThatValue
    :: forall h tail
    .  SumValueL tail
    -> SumValueL (h ': tail)

data ProductValueL :: [(Symbol, Structure)] -> * where
  ProductNil :: ProductValueL '[]
  ProductCons
    :: (KnownSymbol t)
    => Proxy t
    -> StructureValue s
    -> ProductValueL tail
    -> ProductValueL ('(t, s) ': tail)
