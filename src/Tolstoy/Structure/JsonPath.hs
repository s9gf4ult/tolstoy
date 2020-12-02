module Tolstoy.Structure.JsonPath where

import           Data.Proxy
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.TypeLits
import           Tolstoy.Structure.Kind

-- | The path inside some structure. First argument is the structure
-- to query on. The second is the structure the path returns.
data StructurePath :: Structure -> Structure -> * where
  -- | End of the path. Generates nothing
  ThisPath :: StructurePath s s
  -- | Path which do not change the nesting level, but changes the set
  -- of values returned by path. Example: ? (@.key == "some_key")
  FilteredPath :: StructureCondition s -> StructurePath s s
  -- | Path inside of optional value. Generates filtering of null
  -- values "? (@ <> null)"
  OptionalPath :: StructurePath s sub -> StructurePath ('StructOptional s) sub
  -- | Path inside of a vector. Generates "[*]" or "[0]" or "[0-10]"
  -- depending on vector index argument
  VectorPath
    :: VectorIndex
    -> StructurePath s sub
    -> StructurePath ('StructVector s) sub
  -- | Path inside of some sum element.
  -- Generates filtering "? (@.tag == "tag_name").value""
  SumPath :: SumPathTree t sub -> StructurePath ('StructSum t) sub
  -- | Path inside of some product element. Generates simple field
  -- accessor ".field_name"
  ProductPath :: ProductPathTree t sub -> StructurePath ('StructProduct t) sub

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

data VectorIndex
  = VectorAny
  -- ^ "[*]"
  | VectorRange IndexRange
  -- ^ "[0]" or "[0-10]" depending on range

data IndexRange = IndexExact Int | IndexRange Int Int

-- | Note that condition is not just a value of boolean
-- type. Condition is not a value at all. You can use logical
-- operators like "&&" on conditions but not on boolean
-- values. Meaning that the "true && false" is not valid jsonpath
-- construction, but "true == true && false == true" is valid. Also
-- you can not compare conditions with booleans like "(true == false)
-- == false". That is why conditions have distinct type.
data StructureCondition :: Structure -> * where
  -- | "!(true == false)"
  NotCondition :: StructureCondition s -> StructureCondition s
  -- | "? exists(@[*] == true)"
  ExistsCondition :: StructureCondition s -> StructureCondition s
  -- | $.name == "john" && $.last_name == "doe"
  BoolCondition
    :: BoolOperator
    -> StructureCondition s
    -- ^ Left condition
    -> StructureCondition s
    -- ^ Right condition
    -> StructureCondition s
  -- | "string" == "string", or 10 <> 42. Actually the "==" returns
  -- "true" only with two numbers, strings, nulls or booleans. On
  -- objects it always returns "null", on arrays it returns
  -- "false". It always returns "false" on two different types
  EqCondition
    :: Eqable t
    -> EqOperator
    -> StructureJsonValue s ('JsonValueType n t)
    -> StructureJsonValue s ('JsonValueType n t)
    -> StructureCondition s
  -- | Compare two values with different nullability. Sometimes might
  -- help
  EqLaxCondition
    :: Eqable t
    -> EqOperator
    -> StructureJsonValue s ('JsonValueType n1 t)
    -> StructureJsonValue s ('JsonValueType n2 t)
    -> StructureCondition s
  -- | "like_regex" or "starts with". Returns null on null input.
  StringCondition
    :: StructureJsonValue s ('JsonValueType n 'StringType)
    -> StringChecker
    -> StructureCondition s
  -- | 2 > 3. Returns false if any of arguments is null.
  NumberCompareCondition
    :: NumberCompare
    -> StructureJsonValue s ('JsonValueType n1 'NumberType)
    -> StructureJsonValue s ('JsonValueType n2 'NumberType)
    -> StructureCondition s

data EqOperator = EqOperator | NotEqOperator

-- | Restriction of types for "==" operator
data Eqable :: JsonType -> * where
  EqableString :: Eqable 'StringType
  EqableNumber :: Eqable 'NumberType
  EqableNull :: Eqable 'NullType
  EqableBoolean :: Eqable 'BooleanType

-- | Values can be constructed from simple path, but also with some
-- operators and methods. Values can be strict and optional. Optional
-- value can be either null or some other value. Strict value can not
-- be null.
data StructureJsonValue :: Structure -> JsonValueType -> * where
  LiteralStringValue
    :: Text
    -> StructureJsonValue s ('JsonValueType n 'StringType)
  LiteralNumberValue
    :: Scientific
    -> StructureJsonValue s ('JsonValueType n 'NumberType)
  LiteralBoolValue
    :: Bool
    -> StructureJsonValue s ('JsonValueType n 'BooleanType)
  -- | null is the nullable value of any type. Deal with it.
  LiteralNullValue :: StructureJsonValue s ('JsonValueType 'Nullable t)
  -- | Some path value, like "$.a[*].b"
  PathValue :: StructurePath s sub -> StructureJsonValue s (StructValueType sub)
  -- | Two numbers can be combined with one of operators, like "3 + 4"
  -- is also a number. All operators fail on null.
  NumberOperatorValue
    :: NumberOperator
    -> StructureJsonValue s ('JsonValueType 'Strict 'NumberType)
    -> StructureJsonValue s ('JsonValueType 'Strict 'NumberType)
    -> StructureJsonValue s ('JsonValueType 'Strict 'NumberType)
  -- | Call the ".type()" jsonpath function. It always returns the
  -- string on any input value
  TypeOfValue
    :: StructureJsonValue s t
    -> StructureJsonValue s ('JsonValueType 'Strict 'StringType)
  -- | Call the ".size()" jsonpath function. Anways returns
  -- number. The "size" always returns 1 on any non-array value. So we
  -- decline nulls to avoid this weird silent behaviour.
  SizeOfValue
    :: StructureJsonValue s ('JsonValueType 'Strict 'ArrayType)
    -> StructureJsonValue s ('JsonValueType 'Strict 'NumberType)
  -- | Call the ".double()" jsonpath method. Accepts only string or
  -- number. But there is constructor is only for strings. "double"
  -- fails on null so the string must be strict
  StringToDouble
    :: StructureJsonValue s ('JsonValueType 'Strict 'StringType)
    -> StructureJsonValue s ('JsonValueType 'Strict 'NumberType)
  -- | Calls one of the number methods. Accepts number only. All
  -- methods fail if null occured, so it requires strict number
  NumberMethodValue
    :: NumberMethod
    -> StructureJsonValue s ('JsonValueType 'Strict 'NumberType)
    -> StructureJsonValue s ('JsonValueType 'Strict 'NumberType)

data NumberMethod
  = NumberCeiling
  -- ^ ".ceiling()" method
  | NumberFloor
  -- ^ ".floor()" method
  | NumberAbs
  -- ^ ".abs()" method
  | NumberDouble
  -- ^ ".double()" method for numbers

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
  = NumberPlus
  | NumberMinus
  | NumberMultiply
  | NumberDivide
  | NumberModulus

data StringChecker
  = StringLikeRegex Text [RegexFlag]
  | StringStartsWith Text

data RegexFlag
  = RegexIFlag
  -- ^ i for case-insensitive match
  | RegexMFlag
  -- ^ m to allow ^ and $ to match at newlines
  | RegexSFlag
  -- ^ s to allow . to match a newline
  | RegexQFlag
  -- ^ q to quote the whole pattern (reducing the behavior to a simple
  -- substring match).

data NumberCompare
  = NumberLT
  | NumberLE
  | NumberGT
  | NumberGE

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
