{-# LANGUAGE AllowAmbiguousTypes #-}

module Tolstoy.DSL.JsonPath.Build where

import           Data.Function
import           Data.Proxy
import           Data.Scientific
import           Data.Text (Text)
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
optional = QueryPath OptionalPath

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

nullLit :: StructureQuery r c 'StructNull
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

class AnyFieldClass (s :: Structure) where
  anyField :: AnyField s
instance AnyFieldClass ('StructProduct p) where
  anyField = ProdAnyField
instance AnyFieldClass ('StructSum s) where
  anyField = SumAnyField

objAny
  :: forall ret any r c
  .  (KnownStructure (StructKind ret), AnyFieldClass any)
  => StructureQuery r c any
  -> StructureQuery r c (StructKind ret)
objAny = QueryAnyField structureRep anyField

anyRecur
  :: forall ret inner r c
  . (KnownStructure (StructKind ret))
  => Maybe IndexRange
  -> StructureQuery r c inner
  -> StructureQuery r c (StructKind ret)
anyRecur = QueryRecursiveAnyField structureRep

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

class (final ~ EqFinal a b)
  => EqableClass (a :: Structure) (b :: Structure) (final :: Bool) where
  eqable :: Eqable a b
instance EqableClass 'StructString 'StructString 'True where
  eqable = EqableString
instance EqableClass 'StructNumber 'StructNumber 'True where
  eqable = EqableNumber
instance EqableClass 'StructBool 'StructBool 'True where
  eqable = EqableBoolean
instance EqableClass 'StructNull ('StructOptional s) 'True where
  eqable = EqableNullOpt
instance EqableClass ('StructOptional s) 'StructNull 'True where
  eqable = EqableCommut EqableNullOpt
instance (EqableClass a b (EqFinal a b))
  => EqableClass ('StructOptional a) ('StructOptional b) 'False where
  eqable = EqableOptOpt eqable
instance
  ( EqableClass a sub (EqFinal a sub)
  , 'False ~ (EqFinal a ('StructOptional sub))
  ) => EqableClass a ('StructOptional sub) 'False where
  eqable = EqableOpt eqable
instance
  ( EqableClass a sub (EqFinal a sub)
  , 'False ~ EqFinal ('StructOptional sub) a
  ) => EqableClass ('StructOptional sub) a 'False where
  eqable = EqableCommut (EqableOpt eqable)

type family EqFinal a b :: Bool where
  EqFinal ('StructOptional a) ('StructOptional b) = False
  EqFinal 'StructNull ('StructOptional a) = True
  EqFinal ('StructOptional a) 'StructNull = True
  EqFinal a ('StructOptional b) = False
  EqFinal ('StructOptional a) b = False
  EqFinal a a = True

(==:)
  :: (EqableClass a b (EqFinal a b))
  => StructureQuery r c a
  -> StructureQuery r c b
  -> StructureCondition r c
(==:) = EqCondition eqable EqOperator

infix 4 ==:

(<>:)
  :: (EqableClass a b (EqFinal a b))
  => StructureQuery r c a
  -> StructureQuery r c b
  -> StructureCondition r c
(<>:) = EqCondition eqable NotEqOperator

infix 4 <>:

likeRegex
  :: StructureQuery r c 'StructString
  -> T.Text
  -> StructureCondition r c
likeRegex v t = StringCondition v (StringLikeRegex t [])

infix 4 `likeRegex`

startsWith
  :: StructureQuery r c 'StructString
  -> T.Text
  -> StructureCondition r c
startsWith v t = StringCondition v (StringStartsWith t)

infix 4 `startsWith`

(>:)
  :: StructureQuery r c 'StructNumber
  -> StructureQuery r c 'StructNumber
  -> StructureCondition r c
(>:) = NumberCompareCondition NumberLT

infix 4 >:

(>=:)
  :: StructureQuery r c 'StructNumber
  -> StructureQuery r c 'StructNumber
  -> StructureCondition r c
(>=:) = NumberCompareCondition NumberLE

infix 4 >=:

(<:)
  :: StructureQuery r c 'StructNumber
  -> StructureQuery r c 'StructNumber
  -> StructureCondition r c
(<:) = NumberCompareCondition NumberGT

infix 4 <:

(<=:)
  :: StructureQuery r c 'StructNumber
  -> StructureQuery r c 'StructNumber
  -> StructureCondition r c
(<=:) = NumberCompareCondition NumberGE

infix 4 <=:

truthCondition :: StructureCondition r c
truthCondition = boolLit True ==: boolLit True

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
renderCondition c = TB.toLazyText $ Render.renderCondition c
