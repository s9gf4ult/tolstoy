module Tolstoy.Structure.JsonPath where

import           Data.Proxy
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           GHC.TypeLits
import           Tolstoy.Structure.Kind

data StructureQuery
  :: Structure
  -- ^ Root structure of the document. The $ variable in jsonpath
  -> Maybe Structure
  -- ^ Current context structure. The @ variable in jsonpath. The @
  -- variable is not available outside of the condition.
  -> Structure
  -- ^ Structure returning something
  -> * where
  -- | The "$" query
  QueryRoot :: StructureQuery r c r
  -- | The "@" query.
  QueryContext :: StructureQuery r ('Just c) c
  QueryFilter
    :: StructureQuery r c ret
    -- ^ Query before the filter
    -> StructureCondition r ('Just ret)
    -- ^ The filter conditon. Return type of previous query becomes
    -- context of the condition
    -> StructureQuery r c ret
    -- ^ When we exit the filter context doesn't change.
  QueryNesting
    :: ( Show (StructureQuery r c inner)
       , Show (StructurePath inner outer)
       )
    => StructureQuery r c inner
    -- ^ Query before the filter
    -> StructurePath inner outer
    -- ^ Path inside of the
    -> StructureQuery r c outer

deriving instance Show (StructureQuery r c ret)

-- | The path inside some structure. First argument is the structure
-- to query on. The second is the structure the path returns.
data StructurePath
  :: Structure
  -- ^ Structure in current context
  -> Structure
  -- ^ Structure the path returns
  -> * where
  -- | Path inside of optional value. Generates filtering of null
  -- values "? (@ <> null)"
  OptionalPath :: StructurePath ('StructOptional s) s
  -- | Path inside of a vector. Generates "[*]" or "[0]" or "[0-10]"
  -- depending on vector index argument
  VectorPath
    :: VectorIndex
    -> StructurePath ('StructVector s) s
  -- | Path inside of some sum element.
  -- Generates filtering "? (@.tag == "tag_name").value""
  SumPath
    :: (KnownSymbol tag)
    => Proxy tag
    -> SumPathTree tag t sub
    -> StructurePath ('StructSum t) sub
  -- | Path inside of some product element. Generates simple field
  -- accessor ".field_name"
  ProductPath
    :: (KnownSymbol tag)
    => Proxy tag
    -> ProductPathTree tag t sub
    -> StructurePath ('StructProduct t) sub

deriving instance Show (StructurePath c ret)

-- | Subquery on some value of the sum. Example "? (@.tag == "some_tag").value"
data SumPathTree :: Symbol -> SumTree -> Structure -> * where
  Sum1PathTree
    :: SumPathTree tag ('Sum1 tag s) s
  Sum2LeftPathTree
    :: !(SumPathTree tag l sub)
    -> SumPathTree tag ('Sum2 l r) sub
  Sum2RightPathTree
    :: !(SumPathTree tag r sub)
    -> SumPathTree tag ('Sum2 l r) sub

deriving instance Show (SumPathTree tag t s)

-- | Subquery on some field of the product. Example ".field_name"
data ProductPathTree :: Symbol -> ProductTree -> Structure -> * where
  Product1PathTree
    :: ProductPathTree tag ('Product1 tag s) s
  Product2LeftPathTree
    :: ProductPathTree tag l sub
    -> ProductPathTree tag ('Product2 l r) sub
  Product2RightPathTree
    :: ProductPathTree tag r sub
    -> ProductPathTree tag ('Product2 l r) sub

deriving instance Show (ProductPathTree tag t s)

data VectorIndex
  = VectorAny
  -- ^ "[*]"
  | VectorRange IndexRange
  -- ^ "[0]" or "[0-10]" depending on range
  deriving (Eq, Ord, Show, Generic)

data IndexRange = IndexExact Int | IndexRange Int Int
  deriving (Eq, Ord, Show, Generic)

-- | Note that condition is not just a value of boolean
-- type. Condition is not a value at all. You can use logical
-- operators like "&&" on conditions but not on boolean
-- values. Meaning that the "true && false" is not valid jsonpath
-- construction, but "true == true && false == true" is valid. Also
-- you can not compare conditions with booleans like "(true == false)
-- == false". That is why conditions have distinct type.
data StructureCondition
  :: Structure
  -- ^ Root structure
  -> Maybe Structure
  -- ^ Context of the condition. The type of the @ variable. In top
  -- level conditions
  -> * where
  -- | "!(true == false)"
  NotCondition :: StructureCondition r c -> StructureCondition r c
  -- | "? exists(@[*] == true)"
  ExistsCondition :: StructureCondition r c -> StructureCondition r c
  -- | $.name == "john" && $.last_name == "doe"
  BoolCondition
    :: BoolOperator
    -> StructureCondition r c
    -- ^ Left condition
    -> StructureCondition r c
    -- ^ Right condition
    -> StructureCondition r c
  -- | "string" == "string", or 10 <> 42. Actually the "==" returns
  -- "true" only with two numbers, strings, nulls or booleans. On
  -- objects it always returns "null", on arrays it returns
  -- "false". It always returns "false" on two different types
  EqCondition
    :: Eqable t
    -> EqOperator
    -> StructureJsonValue r c ('JsonValueType n1 t)
    -> StructureJsonValue r c ('JsonValueType n2 t)
    -> StructureCondition r c
  -- | "like_regex" or "starts with". Returns null on null input.
  StringCondition
    :: StructureJsonValue r c ('JsonValueType n 'StringType)
    -> StringChecker
    -> StructureCondition r c
  -- | 2 > 3. Returns false if any of arguments is null.
  NumberCompareCondition
    :: NumberCompare
    -> StructureJsonValue r c ('JsonValueType n1 'NumberType)
    -> StructureJsonValue r c ('JsonValueType n2 'NumberType)
    -> StructureCondition r c

deriving instance Show (StructureCondition r c)

data EqOperator = EqOperator | NotEqOperator
  deriving (Eq, Ord, Show, Generic)

-- | Restriction of types for "==" operator
data Eqable :: JsonType -> * where
  EqableString :: Eqable 'StringType
  EqableNumber :: Eqable 'NumberType
  EqableNull :: Eqable 'NullType
  EqableBoolean :: Eqable 'BooleanType

deriving instance Show (Eqable t)

-- | Values can be constructed from simple path, but also with some
-- operators and methods. Values can be strict and optional. Optional
-- value can be either null or some other value. Strict value can not
-- be null.
data StructureJsonValue
  :: Structure
  -- ^ Root
  -> Maybe Structure
  -- ^ Context if available
  -> JsonValueType
  -> * where
  LiteralStringValue
    :: Text
    -> StructureJsonValue r c ('JsonValueType n 'StringType)
  LiteralNumberValue
    :: Scientific
    -> StructureJsonValue r c ('JsonValueType n 'NumberType)
  LiteralBoolValue
    :: Bool
    -> StructureJsonValue r c ('JsonValueType n 'BooleanType)
  -- | null is the nullable value of any type. Deal with it.
  LiteralNullValue :: StructureJsonValue r c ('JsonValueType 'Nullable t)
  -- | Some path value, like "$.a[*].b"
  QueryValue
    :: StructureQuery r c ret
    -> StructureJsonValue r c (StructValueType ret)
  -- | Two numbers can be combined with one of operators, like "3 + 4"
  -- is also a number. All operators fail on null.
  NumberOperatorValue
    :: NumberOperator
    -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
    -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
    -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
  -- | Call the ".type()" jsonpath function. It always returns the
  -- string on any input value
  TypeOfValue
    :: StructureJsonValue r c t
    -> StructureJsonValue r c ('JsonValueType 'Strict 'StringType)
  -- | Call the ".size()" jsonpath function. Anways returns
  -- number. The "size" always returns 1 on any non-array value. So we
  -- decline nulls to avoid this weird silent behaviour.
  SizeOfValue
    :: StructureJsonValue r c ('JsonValueType 'Strict 'ArrayType)
    -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
  -- | Call the ".double()" jsonpath method. Accepts only string or
  -- number. But there is constructor is only for strings. "double"
  -- fails on null so the string must be strict
  StringToDouble
    :: StructureJsonValue r c ('JsonValueType 'Strict 'StringType)
    -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
  -- | Calls one of the number methods. Accepts number only. All
  -- methods fail if null occured, so it requires strict number
  NumberMethodValue
    :: NumberMethod
    -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
    -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
  -- | Selects any field from object.  The ".*" operator.  Returns
  -- literally any value.  Fails on nulls.  The constructor is not
  -- StructurePath because we loose the type of the inner structure.
  -- But we don't want to build untypeable paths. Nullability is set
  -- by default, because we can not guarantee, that there are no nulls
  -- inside of the object.
  ObjectAnyFieldValue
    :: StructureJsonValue r c ('JsonValueType 'Strict 'ObjectType)
    -- ^ The object to nest in
    -> StructureJsonValue r c ('JsonValueType 'Nullable t)
  -- | Selects any field from object, or any element from array with
  -- optional depth. Works with any type, even with null. Returns
  -- literally any type. The constructor is not StructurePath because
  -- we loose the type of the inner structure. But we don't want to
  -- build untypeable paths. Nullability is set by default, because we
  -- can not guarantee, that there are no nulls inside of the value.
  RecursiveElementValue
    :: Maybe IndexRange
    -- ^ The level of nesting to traverse.
    -> StructureJsonValue r c t1
    -- ^ The value to nest in
    -> StructureJsonValue r c ('JsonValueType 'Nullable t2)
  -- | Filter any unknown types to specified one. Generates '?
  -- (@.type() == "string"' for strings for example. Also passess
  -- nulls like '? (@ == null || @.type() == "number")'
  FilterTypeValue
    :: JsonValueTypeRep ret
    -> StructureJsonValue r c t
    -> StructureJsonValue r c ret
  -- | ? (@ <> null)
  FilterStrictValue
    :: StructureJsonValue r c ('JsonValueType n t)
    -> StructureJsonValue r c ('JsonValueType 'Strict t)

deriving instance Show (StructureJsonValue r c t)

data NumberMethod
  = NumberCeiling
  -- ^ ".ceiling()" method
  | NumberFloor
  -- ^ ".floor()" method
  | NumberAbs
  -- ^ ".abs()" method
  | NumberDouble
  -- ^ ".double()" method for numbers
  deriving (Eq, Ord, Show, Generic)

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
  deriving (Eq, Ord, Show, Generic)

data StringChecker
  = StringLikeRegex Text [RegexFlag]
  | StringStartsWith Text
  deriving (Eq, Ord, Show, Generic)

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
  deriving (Eq, Ord, Show, Generic)

data NumberCompare
  = NumberLT
  | NumberLE
  | NumberGT
  | NumberGE
  deriving (Eq, Ord, Show, Generic)

data Nullable = Nullable | Strict
  deriving (Eq, Ord, Show, Generic)

data NullableRep :: Nullable -> * where
  NullableRep :: NullableRep 'Nullable
  StrictRep :: NullableRep 'Strict

deriving instance Show (NullableRep n)

data JsonValueType = JsonValueType Nullable JsonType
  deriving (Eq, Ord, Show, Generic)

data JsonValueTypeRep :: JsonValueType -> * where
  JsonValueTypeRep
    :: NullableRep n
    -> JsonTypeRep t
    -> JsonValueTypeRep ('JsonValueType n t)

deriving instance Show (JsonValueTypeRep t)

data JsonType
  = StringType
  | NumberType
  | ObjectType
  | ArrayType
  | NullType
  | BooleanType
  deriving (Eq, Ord, Show, Generic)

data JsonTypeRep :: JsonType -> * where
  StringTypeRep :: JsonTypeRep 'StringType
  NumberTypeRep :: JsonTypeRep 'NumberType
  ObjectTypeRep :: JsonTypeRep 'ObjectType
  ArrayTypeRep :: JsonTypeRep 'ArrayType
  NullTypeRep :: JsonTypeRep 'NullType
  BooleanTypeRep :: JsonTypeRep 'BooleanType

deriving instance Show (JsonTypeRep t)

data BoolOperator = BoolAnd | BoolOr
  deriving (Eq, Ord, Show, Generic)
