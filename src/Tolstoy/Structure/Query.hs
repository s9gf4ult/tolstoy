module Tolstoy.Structure.Query where

import Data.Proxy
import GHC.TypeLits
import Tolstoy.Structure.Kind

-- | The path inside some structure. First argument is the structure
-- to query on. The second is the structure the path returns.
data StructurePath :: Structure -> Structure -> * where
  ThisPath :: StructurePath s s
  OptionalPath :: StructurePath s sub -> StructurePath ('StructOptional s) sub
  VectorPath :: VectorIndex -> StructurePath s sub -> StructurePath ('StructVector s) sub
  SumPath :: SumPathTree t sub -> StructurePath ('StructSum t) sub
  ProductPath :: ProductPathTree t sub -> StructurePath ('StructProduct t) sub

data VectorIndex = VectorAny | VectorRange IndexRange

data IndexRange = IndexThis Int | IndexRange Int Int

-- | Subquery on some value of the sum. Example "? (@.tag == "some_tag").value"
data SumPathTree :: SumTree -> Structure -> * where
  Sum1PathTree
    :: (KnownSymbol n)
    => Proxy n
    -> !(StructurePath s sub)
    -> SumPathTree ('Sum1 n s) sub
  Sum2LeftPathTree
    :: !(SumPathTree l sub)
    -> SumPathTree ('Sum2 l r) sub
  Sum2RightPathTree
    :: !(SumPathTree r sub)
    -> SumPathTree ('Sum2 l r) sub

-- | Subquery on some field of the product. Example ".field_name"
data ProductPathTree :: ProductTree -> Structure -> * where
  Product1PathTree
    :: (KnownSymbol n)
    => Proxy n
    -> StructurePath s sub
    -> ProductPathTree ('Product1 n s) sub
  Product2LeftPathTree
    :: ProductPathTree l sub
    -> ProductPathTree ('Product2 l r) sub
  Product2RightPathTree
    :: ProductPathTree r sub
    -> ProductPathTree ('Product2 l r) sub

data StructureCondition :: Structure -> * where
  NotCondition :: StructureCondition s -> StructureCondition s
  ExistsCondition :: StructureCondition s -> StructureCondition s
  BinaryCondition
    :: BoolOperator
    -> StructureCondition s
    -- ^ Left condition
    -> StructureCondition s
    -- ^ Right condition
    -> StructureCondition s
  StringCondition
    :: StructureJsonValue s 'StringType
    -> StringChecker
    -> StructureCondition s
  NumberCondition
    :: StructureJsonValue s 'NumberType
    -> NumberChecker
    -> StructureCondition s
  BoolEqCondition
    :: StructureJsonValue s 'BooleanType
    -> Bool
    -> StructureCondition s

data StructureJsonValue :: Structure -> JsonType -> * where

data StringChecker

data NumberChecker

data JsonType
  = StringType
  | NumberType
  | ObjectType
  | ArrayType
  | NullType
  | BooleanType

data BoolOperator = BoolAnd | BoolOr
