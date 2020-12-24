{-# LANGUAGE AllowAmbiguousTypes #-}

module Tolstoy.DSL.JsonPath.Build where

import           Data.Function
import           Data.Proxy
import           Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import           GHC.TypeLits
import           Tolstoy.DSL.JsonPath.Render as Render
import           Tolstoy.Structure

-- Query

root :: StructureQuery r c r
root = QueryRoot

ctx :: StructureQuery r ('Just c) c
ctx = QueryContext

(?:)
  :: StructureQuery r c ret
  -> StructureCondition r ('Just ret)
  -> StructureQuery r c ret
(?:) q co = QueryFilter co q

infixl 7 ?:

(.:)
  :: StructureQuery r c a
  -> (StructureQuery r c a -> StructureQuery r c b)
  -> StructureQuery r c b
(.:) = (&)

infixl 7 .:

-- Query Path

optional :: StructureQuery r c ('StructOptional s) -> StructureQuery r c s
optional q = QueryPath OptionalPath

vectorAny :: StructureQuery r c ('StructVector s) -> StructureQuery r c s
vectorAny = QueryPath $ VectorPath VectorAny

vectorIndex
  :: Int
  -> StructureQuery r c ('StructVector s)
  -> StructureQuery r c s
vectorIndex i = QueryPath $ VectorPath $ VectorRange $ IndexExact i

vectorRange
  :: Int
  -> Int
  -> StructureQuery r c ('StructVector s)
  -> StructureQuery r c s
vectorRange a b = QueryPath $ VectorPath $ VectorRange $ IndexRange a b

class (g ~ WhereSum tree ('Sum1 tag sub))
  => KnownSumPathTree (tag :: Symbol) (tree :: SumTree) (sub :: Structure) (g :: Maybe Goto)where
  knownSumPathTree :: SumPathTree tag tree sub

instance KnownSumPathTree tag ('Sum1 tag s) s ('Just 'C) where
  knownSumPathTree = Sum1PathTree

instance
  ( ('Just 'L) ~ WhereSum ('Sum2 l r) ('Sum1 tag sub)
  , KnownSumPathTree tag l sub (WhereSum l ('Sum1 tag sub))
  ) => KnownSumPathTree tag ('Sum2 l r) sub ('Just 'L) where
  knownSumPathTree = Sum2LeftPathTree knownSumPathTree

instance
  ( ('Just 'R) ~ WhereSum ('Sum2 l r) ('Sum1 tag sub)
  , KnownSumPathTree tag r sub (WhereSum r ('Sum1 tag sub))
  ) => KnownSumPathTree tag ('Sum2 l r) sub ('Just 'R) where
  knownSumPathTree = Sum2RightPathTree knownSumPathTree

sumElem
  :: forall tag sub tree r c
  . ( KnownSumPathTree tag tree sub (WhereSum tree ('Sum1 tag sub))
    , KnownSymbol tag
    , sub ~ (SumElem tree tag)
    )
  => StructureQuery r c ('StructSum tree)
  -> StructureQuery r c sub
sumElem = QueryPath $ SumPath (Proxy @tag) knownSumPathTree

class (g ~ WhereProd tree ('Product1 tag sub))
  => KnownProductPathTree (tag :: Symbol) (tree :: ProductTree) (sub :: Structure) (g :: Maybe Goto)where
  knownProductPathTree :: ProductPathTree tag tree sub

instance KnownProductPathTree tag ('Product1 tag s) s ('Just 'C) where
  knownProductPathTree = Product1PathTree

instance
  ( ('Just 'L) ~ WhereProd ('Product2 l r) ('Product1 tag sub)
  , KnownProductPathTree tag l sub (WhereProd l ('Product1 tag sub))
  ) => KnownProductPathTree tag ('Product2 l r) sub ('Just 'L) where
  knownProductPathTree = Product2LeftPathTree knownProductPathTree

instance
  ( ('Just 'R) ~ WhereProd ('Product2 l r) ('Product1 tag sub)
  , KnownProductPathTree tag r sub (WhereProd r ('Product1 tag sub))
  ) => KnownProductPathTree tag ('Product2 l r) sub ('Just 'R) where
  knownProductPathTree = Product2RightPathTree knownProductPathTree

prodElem
  :: forall tag sub tree r c
  . ( KnownProductPathTree tag tree sub (WhereProd tree ('Product1 tag sub))
    , KnownSymbol tag
    , sub ~ (ProdElem tree tag)
    )
  => StructureQuery r c ('StructProduct tree)
  -> StructureQuery r c sub
prodElem = QueryPath $ ProductPath (Proxy @tag) knownProductPathTree

numLit :: Scientific -> StructureQuery r c 'StructNumber
numLit = QueryLiteral . LiteralNumber

textLit :: Text -> StructureQuery r c 'StructString
textLit = QueryLiteral . LiteralString

boolLit :: Bool -> StructureQuery r c 'StructBool
boolLit = QueryLiteral . LiteralBool

nullLit :: StructureQuery r c ('StructOptional s)
nullLit = QueryLiteral LiteralNull

(+:)
  :: StructureQuery r c 'StructNumber
  -> StructureQuery r c 'StructNumber
  -> StructureQuery r c 'StructNumber
(+:) = QueryNumberOperator NumberPlus

infixl 5 +:

(-:)
  :: StructureQuery r c 'StructNumber
  -> StructureQuery r c 'StructNumber
  -> StructureQuery r c 'StructNumber
(-:) = QueryNumberOperator NumberMinus

infixl 5 -:

(*:)
  :: StructureQuery r c 'StructNumber
  -> StructureQuery r c 'StructNumber
  -> StructureQuery r c 'StructNumber
(*:) = QueryNumberOperator NumberMultiply

infixl 6 *:

(/:)
  :: StructureQuery r c 'StructNumber
  -> StructureQuery r c 'StructNumber
  -> StructureQuery r c 'StructNumber
(/:) = QueryNumberOperator NumberDivide

infixl 6 /:

(%:)
  :: StructureQuery r c 'StructNumber
  -> StructureQuery r c 'StructNumber
  -> StructureQuery r c 'StructNumber
(%:) = QueryNumberOperator NumberModulus

infixl 6 %:

dotType :: StructureQuery r c any -> StructureQuery r c 'StructString
dotType = QueryMethodCall CallType

dotSize
  :: StructureQuery r c ('StructVector any)
  -> StructureQuery r c 'StructNumber
dotSize = QueryMethodCall CallSize

class DoubleCompatClass (t :: Structure) where
  doubleCompat :: DoubleCompat t
instance DoubleCompatClass 'StructNumber where
  doubleCompat = DoubleNumber
instance DoubleCompatClass 'StructString where
  doubleCompat = DoubleString

dotDouble
  :: (DoubleCompatClass any)
  => StructureQuery r c any
  -> StructureQuery r c 'StructNumber
dotDouble = QueryMethodCall $ CallDouble doubleCompat

dotCeiling
  :: StructureQuery r c 'StructNumber
  -> StructureQuery r c 'StructNumber
dotCeiling = QueryMethodCall $ CallNumberMethod NumberCeiling

dotFloor
  :: StructureQuery r c 'StructNumber
  -> StructureQuery r c 'StructNumber
dotFloor = QueryMethodCall $ CallNumberMethod NumberFloor

dotAbs
  :: StructureQuery r c 'StructNumber
  -> StructureQuery r c 'StructNumber
dotAbs = QueryMethodCall $ CallNumberMethod NumberAbs

dotStar
  :: ()
    StructureQuery r c any
  -> StructureQuery r c ret
dotStar = ObjectAnyFieldValue

dotStarStar
  :: Maybe IndexRange
  -- ^ The level of nesting to traverse.
  -> StructureJsonValue r c t1
  -- ^ The value to nest in
  -> StructureJsonValue r c ('JsonValueType 'Nullable t2)
dotStarStar = RecursiveElementValue


-- Condition

cNot :: StructureCondition r c -> StructureCondition r c
cNot = NotCondition

cExists :: StructureCondition r c -> StructureCondition r c
cExists = ExistsCondition

(&&:)
  :: StructureCondition r c
  -> StructureCondition r c
  -> StructureCondition r c
(&&:) = BoolCondition BoolAnd

infixr 3 &&:

(||:)
  :: StructureCondition r c
  -> StructureCondition r c
  -> StructureCondition r c
(||:) = BoolCondition BoolOr

infixr 2 ||:

class EqableType (t :: JsonType) where
  eqable :: Eqable t
instance EqableType 'StringType where
  eqable = EqableString
instance EqableType 'NumberType where
  eqable = EqableNumber
instance EqableType 'NullType where
  eqable = EqableNull
instance EqableType 'BooleanType where
  eqable = EqableBoolean

(==:)
  :: (EqableType t)
  => StructureJsonValue r c ('JsonValueType n1 t)
  -> StructureJsonValue r c ('JsonValueType n2 t)
  -> StructureCondition r c
(==:) = EqCondition eqable EqOperator

infix 4 ==:

(<>:)
  :: (EqableType t)
  => StructureJsonValue r c ('JsonValueType n1 t)
  -> StructureJsonValue r c ('JsonValueType n2 t)
  -> StructureCondition r c
(<>:) = EqCondition eqable NotEqOperator

infix 4 <>:

like_regex
  :: StructureJsonValue r c ('JsonValueType n 'StringType)
  -> T.Text
  -> StructureCondition r c
like_regex v t = StringCondition v (StringLikeRegex t [])

infix 4 `like_regex`

starts_with
  :: StructureJsonValue r c ('JsonValueType n 'StringType)
  -> T.Text
  -> StructureCondition r c
starts_with v t = StringCondition v (StringStartsWith t)

infix 4 `starts_with`

(>:)
  :: StructureJsonValue r c ('JsonValueType n1 'NumberType)
  -> StructureJsonValue r c ('JsonValueType n2 'NumberType)
  -> StructureCondition r c
(>:) = NumberCompareCondition NumberLT

infix 4 >:

(>=:)
  :: StructureJsonValue r c ('JsonValueType n1 'NumberType)
  -> StructureJsonValue r c ('JsonValueType n2 'NumberType)
  -> StructureCondition r c
(>=:) = NumberCompareCondition NumberLE

infix 4 >=:

(<:)
  :: StructureJsonValue r c ('JsonValueType n1 'NumberType)
  -> StructureJsonValue r c ('JsonValueType n2 'NumberType)
  -> StructureCondition r c
(<:) = NumberCompareCondition NumberGT

infix 4 <:

(<=:)
  :: StructureJsonValue r c ('JsonValueType n1 'NumberType)
  -> StructureJsonValue r c ('JsonValueType n2 'NumberType)
  -> StructureCondition r c
(<=:) = NumberCompareCondition NumberGE

infix 4 <=:

-- Value

class JsonValueLiteral typ (n :: Nullable) (t :: JsonType) where
  lit :: typ -> StructureJsonValue r c ('JsonValueType n t)
instance JsonValueLiteral T.Text 'Strict 'StringType where
  lit t = LiteralStringValue t
instance JsonValueLiteral Scientific 'Strict 'NumberType where
  lit v = LiteralNumberValue v
instance JsonValueLiteral Bool 'Strict 'BooleanType where
  lit v = LiteralBoolValue v

data Null = Null

instance JsonValueLiteral Null 'Nullable t where
  lit _ = LiteralNullValue

query
  :: StructureQuery r c ret
  -> StructureJsonValue r c (StructValueType ret)
query = QueryValue

-- typeOf
--   :: StructureJsonValue r c t
--   -> StructureJsonValue r c ('JsonValueType 'Strict 'StringType)
-- typeOf = TypeOfValue

-- sizeOf
--   :: StructureJsonValue r c ('JsonValueType 'Strict 'ArrayType)
--   -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
-- sizeOf = SizeOfValue

-- stringToDouble
--   :: StructureJsonValue r c ('JsonValueType 'Strict 'StringType)
--   -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
-- stringToDouble = StringToDouble

-- ceiling
--   :: StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
--   -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
-- ceiling = NumberMethodValue NumberCeiling

-- floor
--   :: StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
--   -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
-- floor = NumberMethodValue NumberFloor

-- abs
--   :: StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
--   -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
-- abs = NumberMethodValue NumberAbs

-- double
--   :: StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
--   -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
-- double = NumberMethodValue NumberDouble

-- filterType
--   :: JsonValueTypeRep ret
--   -> StructureJsonValue r c t
--   -> StructureJsonValue r c ret
-- filterType = FilterTypeValue

-- filterStrict
--   :: StructureJsonValue r c ('JsonValueType n t)
--   -> StructureJsonValue r c ('JsonValueType 'Strict t)
-- filterStrict = FilterStrictValue

class ToStructureJsonValue a r c t | a -> r c t where
  toStructureJsonValue :: a -> StructureJsonValue r c t
instance ToStructureJsonValue (StructureJsonValue r c t) r c t where
  toStructureJsonValue = id
instance (t ~ (StructValueType ret))
  => ToStructureJsonValue (StructureQuery r c ret) r c t where
  toStructureJsonValue = query

(&:)
  :: (ToStructureJsonValue a r c t)
  => a
  -> (StructureJsonValue r c t -> StructureJsonValue r c t2)
  -> StructureJsonValue r c t2
(&:) a f = f (toStructureJsonValue a)

infixl 7 &:

-- Render

renderQuery
  :: forall (root :: *) (ret :: Structure)
  . (Structural root)
  => StructureQuery (StructKind root) 'Nothing ret
  -> TL.Text
renderQuery q = TB.toLazyText $ Render.renderQuery q

renderCondition
  :: forall root
  . (Structural root)
  => StructureCondition (StructKind root) 'Nothing
  -> TL.Text
renderCondition c = TB.toLazyText $ Render.renderCond c
