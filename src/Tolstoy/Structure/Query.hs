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
    :: StructureJsonValue s ('JsonValueType n 'StringType)
    -> StringChecker
    -> StructureCondition s
  NumberCondition
    :: StructureJsonValue s ('JsonValueType n 'NumberType)
    -> NumberChecker
    -> StructureCondition s
  BoolEqCondition
    :: StructureJsonValue s ('JsonValueType n 'BooleanType)
    -> Bool
    -> StructureCondition s
  NullableCondition
    :: StructureJsonValue s ('JsonValueType 'Nullable t)
    -> Nullable
    -- ^ Here 'Nullable' turns into "== null" and 'Strict' turns into
    -- "<> null"
    -> StructureCondition s

data StructureJsonValue :: Structure -> JsonValueType -> * where
  PathValue :: StructurePath s sub -> StructureJsonValue s (StructValueType sub)
  NumberOperatorValue
    :: NumberOperator
    -> StructureJsonValue s ('JsonValueType n1 'NumberType)
    -> StructureJsonValue s ('JsonValueType n2 'NumberType)
    -> StructureJsonValue s ('JsonValueType (N2 n1 n2) 'NumberType)
  -- | Call the ".type()" jsonpath function. It always returns the string
  TypeOfValue
    :: StructureJsonValue s t
    -> StructureJsonValue s ('JsonValueType 'Strict 'StringType)
  -- | Call the ".size()" jsonpath function. Anways returns number
  SizeOfValue
    :: StructureJsonValue s ('JsonValueType 'Strict 'ArrayType)
    -> StructureJsonValue s ('JsonValueType 'Strict 'NumberType)
  -- | Call the ".double()" jsonpath function. Accepts only string or
  -- number. But there is no reason to call it on numbers. So we are
  -- restricting it on strings only.
  DoubleValue
    :: StructureJsonValue s ('JsonValueType 'Strict 'StringType)
    -> StructureJsonValue s ('JsonValueType 'Strict 'NumberType)
  -- | Calls one of the number methods. Accepts number only
  NumberMethodValue
    :: NumberMethod
    -> StructureJsonValue s ('JsonValueType 'Strict 'NumberType)
    -> StructureJsonValue s ('JsonValueType 'Strict 'NumberType)

data NumberMethod = Ceiling | Floor | Abs

type family StructType (s :: Structure) :: JsonType where
  StructType StructString = 'StringType
  StructType StructNumber = 'NumberType
  StructType StructBool = 'BooleanType
  StructType (StructOptional s) = StructType s
  StructType (StructVector s) = 'ArrayType
  StructType (StructSum s) = 'ObjectType
  StructType (StructProduct s) = 'ObjectType

type family StructValueType (s :: Structure) :: JsonValueType where
  StructValueType ('StructOptional s) = 'JsonValueType 'Nullable (StructType s)
  StructValueType s = 'JsonValueType 'Strict (StructType s)

type family N2 (n1 :: Nullable) (n2 :: Nullable) :: Nullable where
  N2 'Nullable n2 = 'Nullable
  N2 n1 'Nullable = 'Nullable
  N2 n1 n2 = 'Strict

data NumberOperator

data StringChecker

data NumberChecker

data Nullable = Nullable | Strict

data JsonValueType = JsonValueType Nullable JsonType

data JsonType
  = StringType
  | NumberType
  | ObjectType
  | ArrayType
  | NullType
  | BooleanType

data BoolOperator = BoolAnd | BoolOr
