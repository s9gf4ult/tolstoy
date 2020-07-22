module Tolstoy.Structure.Rep where

import           Data.Aeson (FromJSON(..), ToJSON(..), Value, (.:), (.=))
import qualified Data.Aeson as J
import           Data.Aeson.Types (Pair, Parser)
import           Data.Functor ((<$>))
import           Data.Maybe
import           Data.Proxy
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           GHC.TypeLits
import           Prelude (error, fail, mconcat, pure, ($), (++), (==))
import qualified Prelude as P
import           Tolstoy.Structure.Kind

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
