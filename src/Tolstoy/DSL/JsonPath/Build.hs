{-# LANGUAGE AllowAmbiguousTypes #-}

module Tolstoy.DSL.JsonPath.Build where

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
(?:) = QueryFilter

infixl 7 ?:

(.:)
  :: StructureQuery r c inner
  -> StructurePath inner outer
  -> StructureQuery r c outer
(.:) = QueryNesting

infixl 8 .:

-- Path

optional :: StructurePath ('StructOptional s) s
optional = OptionalPath

vectorAny :: StructurePath ('StructVector s) s
vectorAny = VectorPath VectorAny

vectorIndex :: Int -> StructurePath ('StructVector s) s
vectorIndex i = VectorPath $ VectorRange $ IndexExact i

vectorRange :: Int -> Int -> StructurePath ('StructVector s) s
vectorRange a b = VectorPath $ VectorRange $ IndexRange a b

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
  :: forall tag sub tree
  . ( KnownSumPathTree tag tree sub (WhereSum tree ('Sum1 tag sub))
    , KnownSymbol tag
    , sub ~ (SumElem tree tag)
    )
  => StructurePath ('StructSum tree) sub
sumElem = SumPath (Proxy @tag) knownSumPathTree

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
  :: forall tag sub tree
  . ( KnownProductPathTree tag tree sub (WhereProd tree ('Product1 tag sub))
    , KnownSymbol tag
    , sub ~ (ProdElem tree tag)
    )
  => StructurePath ('StructProduct tree) sub
prodElem = ProductPath (Proxy @tag) knownProductPathTree

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

infixr 5 &&:

(||:)
  :: StructureCondition r c
  -> StructureCondition r c
  -> StructureCondition r c
(||:) = BoolCondition BoolOr

infixr 4 ||:

(==:)
  :: StructureJsonValue r c ('JsonValueType n1 t)
  -> StructureJsonValue r c ('JsonValueType n2 t)
  -> StructureCondition r c
(==:) = error "FIXME: (==:) not implemented"

infix 6 ==:

(<>:)
  :: StructureJsonValue r c ('JsonValueType n1 t)
  -> StructureJsonValue r c ('JsonValueType n2 t)
  -> StructureCondition r c
(<>:) = error "FIXME: (<>:) not implemented"

infix 6 <>:

like_regex
  :: StructureJsonValue r c ('JsonValueType n 'StringType)
  -> T.Text
  -> StructureCondition r c
like_regex v t = StringCondition v (StringLikeRegex t [])

infix 6 `like_regex`

starts_with
  :: StructureJsonValue r c ('JsonValueType n 'StringType)
  -> T.Text
  -> StructureCondition r c
starts_with v t = StringCondition v (StringStartsWith t)

infix 6 `starts_with`

(>:)
  :: StructureJsonValue r c ('JsonValueType n1 'NumberType)
  -> StructureJsonValue r c ('JsonValueType n2 'NumberType)
  -> StructureCondition r c
(>:) = NumberCompareCondition NumberLT

(>=:)
  :: StructureJsonValue r c ('JsonValueType n1 'NumberType)
  -> StructureJsonValue r c ('JsonValueType n2 'NumberType)
  -> StructureCondition r c
(>=:) = NumberCompareCondition NumberLE

(<:)
  :: StructureJsonValue r c ('JsonValueType n1 'NumberType)
  -> StructureJsonValue r c ('JsonValueType n2 'NumberType)
  -> StructureCondition r c
(<:) = NumberCompareCondition NumberGT

(<=:)
  :: StructureJsonValue r c ('JsonValueType n1 'NumberType)
  -> StructureJsonValue r c ('JsonValueType n2 'NumberType)
  -> StructureCondition r c
(<=:) = NumberCompareCondition NumberGE

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

(+:)
  :: StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
  -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
  -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
(+:) = NumberOperatorValue NumberPlus

(-:)
  :: StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
  -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
  -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
(-:) = NumberOperatorValue NumberMinus

(*:)
  :: StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
  -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
  -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
(*:) = NumberOperatorValue NumberMultiply

(/:)
  :: StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
  -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
  -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
(/:) = NumberOperatorValue NumberDivide

(%:)
  :: StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
  -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
  -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
(%:) = NumberOperatorValue NumberModulus

typeOf
  :: StructureJsonValue r c t
  -> StructureJsonValue r c ('JsonValueType 'Strict 'StringType)
typeOf = TypeOfValue

sizeOf
  :: StructureJsonValue r c ('JsonValueType 'Strict 'ArrayType)
  -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
sizeOf = error "FIXME: sizeOf not implemented"

stringToDouble
  :: StructureJsonValue r c ('JsonValueType 'Strict 'StringType)
  -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
stringToDouble = StringToDouble

ceiling
  :: StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
  -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
ceiling = NumberMethodValue NumberCeiling

floor
  :: StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
  -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
floor = NumberMethodValue NumberFloor

abs
  :: StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
  -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
abs = NumberMethodValue NumberAbs

double
  :: StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
  -> StructureJsonValue r c ('JsonValueType 'Strict 'NumberType)
double = NumberMethodValue NumberDouble

dotStar
  :: StructureJsonValue r c ('JsonValueType 'Strict 'ObjectType)
  -> StructureJsonValue r c ('JsonValueType 'Nullable t)
dotStar = ObjectAnyFieldValue

dotStarStar
  :: Maybe IndexRange
  -- ^ The level of nesting to traverse.
  -> StructureJsonValue r c t1
  -- ^ The value to nest in
  -> StructureJsonValue r c ('JsonValueType 'Nullable t2)
dotStarStar = RecursiveElementValue

filterType
  :: JsonValueTypeRep ret
  -> StructureJsonValue r c t
  -> StructureJsonValue r c ret
filterType = FilterTypeValue

filterStrict
  :: StructureJsonValue r c ('JsonValueType n t)
  -> StructureJsonValue r c ('JsonValueType 'Strict t)
filterStrict = FilterStrictValue

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
