module Tolstoy.Migration where

import           Data.Aeson (Value, (.=))
import qualified Data.Aeson as J
import           Data.Aeson.Types (Pair)
import           Data.Functor ((<$>))
import           Data.Maybe
import           Data.Proxy
import           Data.Scientific
import           Data.Text as T
import qualified Data.Vector as V
import           GHC.TypeLits
import           Prelude (error, mconcat, pure, ($), (++))
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

instance J.ToJSON (StructureRep s) where
  toJSON s = J.object $ mconcat
    [ pure $ "type" .= stype
    , ("argument" .=) <$> larg
    , ("tags" .=) <$> tags
    ]
    where
      stype :: Text
      stype = case s of
        StringRep      -> "string"
        NumberRep      -> "number"
        BoolRep        -> "bool"
        NullRep        -> "null"
        OptionalRep {} -> "optional"
        VectorRep {}   -> "vector"
        SumRep    {}   -> "sum"
        ProductRep {}  -> "product"
      larg :: [Value]
      larg = case s of
        OptionalRep sub -> pure $ J.toJSON sub
        VectorRep sub   -> pure $ J.toJSON sub
        _               -> []
      tags :: [Value]
      tags = case s of
        SumRep l     -> pure $ J.object $ taggedListJson l
        ProductRep l -> pure $ J.object $ taggedListJson l
        _            -> []

taggedListJson :: TaggedListRep l -> [Pair]
taggedListJson = \case
  TaggedListNil -> []
  TaggedListCons p rep tail ->
    ((T.pack $ symbolVal p) .= rep)
    : taggedListJson tail

-- | Materialize any structure type to it's representation
class KnownStructure (s :: Structure) where
  structureRep :: StructureRep s

instance KnownStructure String where
  structureRep = StringRep

instance KnownStructure Number where
  structureRep = NumberRep

instance KnownStructure Bool where
  structureRep = BoolRep

instance KnownStructure Null where
  structureRep = NullRep

instance (KnownStructure s) => KnownStructure (Optional s) where
  structureRep = OptionalRep structureRep

instance (KnownStructure s) => KnownStructure (Vector s) where
  structureRep = VectorRep structureRep

instance (KnownTaggedList l) => KnownStructure (Sum l) where
  structureRep = SumRep taggedListRep

instance (KnownTaggedList l) => KnownStructure (Product l) where
  structureRep = ProductRep taggedListRep

class KnownTaggedList (l :: [(Symbol, Structure)]) where
  taggedListRep :: TaggedListRep l

instance KnownTaggedList '[] where
  taggedListRep = TaggedListNil

instance (KnownTaggedList tail, KnownStructure s, KnownSymbol t)
  => KnownTaggedList ('(t, s) ': tail) where
  taggedListRep = TaggedListCons (Proxy @t) structureRep taggedListRep

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
