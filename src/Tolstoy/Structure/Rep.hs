{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Tolstoy.Structure.Rep where

import           Data.Aeson
import           Data.Aeson.Types (Pair)
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.TypeLits
import           Tolstoy.Structure.Kind

data StructureRep :: Structure -> * where
  StringRep   :: StructureRep StructString
  NumberRep   :: StructureRep StructNumber
  BoolRep     :: StructureRep StructBool
  OptionalRep :: !(StructureRep s) -> StructureRep (StructOptional s)
  VectorRep   :: !(StructureRep s) -> StructureRep (StructVector s)
  SumRep      :: !(SumTreeRep t) -> StructureRep (StructSum t)
  ProductRep  :: !(ProductTreeRep t) -> StructureRep (StructProduct t)

deriving instance Show (StructureRep s)

data SumTreeRep :: SumTree -> * where
  Sum1Rep
    :: (KnownSymbol t)
    => Proxy t
    -> !(StructureRep s)
    -> SumTreeRep ('Sum1 t s)
  Sum2Rep
    :: !(SumTreeRep t1)
    -> !(SumTreeRep t2)
    -> SumTreeRep ('Sum2 t1 t2)

deriving instance Show (SumTreeRep s)

data ProductTreeRep :: ProductTree -> * where
  Product0Rep :: ProductTreeRep 'Product0
  Product1Rep
    :: (KnownSymbol t)
    => Proxy t
    -> !(StructureRep s)
    -> ProductTreeRep ('Product1 t s)
  Product2Rep
    :: !(ProductTreeRep t1)
    -> !(ProductTreeRep t2)
    -> ProductTreeRep ('Product2 t1 t2)

deriving instance Show (ProductTreeRep p)

instance ToJSON (StructureRep s) where
  toJSON s = object $ mconcat
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
        OptionalRep {} -> "optional"
        VectorRep {}   -> "vector"
        SumRep    {}   -> "sum"
        ProductRep {}  -> "product"
      larg :: [Value]
      larg = case s of
        OptionalRep sub -> pure $ toJSON sub
        VectorRep sub   -> pure $ toJSON sub
        _               -> []
      tags :: [Value]
      tags = case s of
        SumRep t     -> pure $ toJSON t
        ProductRep t -> pure $ toJSON t
        _            -> []

instance ToJSON (SumTreeRep s) where
  toJSON s = object $ sumTreeRepPairs s

sumTreeRepPairs :: SumTreeRep s -> [Pair]
sumTreeRepPairs = \case
  Sum1Rep p s -> [ (T.pack $ symbolVal p) .= s ]
  Sum2Rep a b -> sumTreeRepPairs a ++ sumTreeRepPairs b

instance ToJSON (ProductTreeRep t) where
  toJSON t = object $ productTreeRepPairs t

productTreeRepPairs :: ProductTreeRep t -> [Pair]
productTreeRepPairs = \case
  Product0Rep -> []
  Product1Rep p s -> [ (T.pack $ symbolVal p) .= s ]
  Product2Rep a b -> productTreeRepPairs a ++ productTreeRepPairs b


-- | Materialize any structure type to it's representation
class KnownStructure (s :: Structure) where
  structureRep :: StructureRep s

instance KnownStructure StructString where
  structureRep = StringRep

instance KnownStructure StructNumber where
  structureRep = NumberRep

instance KnownStructure StructBool where
  structureRep = BoolRep

instance (KnownStructure s) => KnownStructure (StructOptional s) where
  structureRep = OptionalRep structureRep

instance (KnownStructure s) => KnownStructure (StructVector s) where
  structureRep = VectorRep structureRep

instance (KnownSumRep l) => KnownStructure (StructSum l) where
  structureRep = SumRep sumRep

instance (KnownProductRep l) => KnownStructure (StructProduct l) where
  structureRep = ProductRep productRep

class KnownSumRep (l :: SumTree) where
  sumRep :: SumTreeRep l

instance
  ( KnownSymbol n
  , KnownStructure s
  ) => KnownSumRep (Sum1 n s) where
  sumRep = Sum1Rep (Proxy @n) structureRep

instance
  ( KnownSumRep t1
  , KnownSumRep t2
  ) => KnownSumRep (Sum2 t1 t2) where
  sumRep = Sum2Rep sumRep sumRep

class KnownProductRep (t :: ProductTree) where
  productRep :: ProductTreeRep t

instance KnownProductRep 'Product0 where
  productRep = Product0Rep

instance
  ( KnownSymbol n
  , KnownStructure s
  ) => KnownProductRep ('Product1 n s) where
  productRep = Product1Rep (Proxy @n) structureRep

instance
  ( KnownProductRep t1
  , KnownProductRep t2
  ) => KnownProductRep ('Product2 t1 t2) where
  productRep = Product2Rep productRep productRep
