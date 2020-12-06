{-# LANGUAGE AllowAmbiguousTypes #-}

module Tolstoy.DSL.JsonPath.Build where

import           Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import           Tolstoy.DSL.JsonPath.Render as Render
import           Tolstoy.Structure
import           Tolstoy.Structure.JsonPath

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

-- Condition

cNot :: StructureCondition r c -> StructureCondition r c
cNot = NotCondition

cExists :: StructureCondition r c -> StructureCondition r c
cExists = ExistsCondition

(&&:)
  :: StructureCondition r c
  -> StructureCondition r c
  -> StructureCondition r c
(&&:) = error "FIXME: (&&:) not implemented"

infixr 5 &&:

(||:)
  :: StructureCondition r c
  -> StructureCondition r c
  -> StructureCondition r c
(||:) = error "FIXME: (||:) not implemented"

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
  :: [RegexFlag]
  -> StructureJsonValue r c ('JsonValueType n 'StringType)
  -> T.Text
  -> StructureCondition r c
like_regex flags v t = StringCondition v (StringLikeRegex t flags)

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


-- Render

renderQuery
  :: forall root ret
  . (Structural root, Structural ret)
  => StructureQuery (StructKind root) 'Nothing (StructKind ret)
  -> TL.Text
renderQuery q = TB.toLazyText $ Render.renderQuery q

renderCondition
  :: forall root
  . (Structural root)
  => StructureCondition (StructKind root) 'Nothing
  -> TL.Text
renderCondition c = TB.toLazyText $ Render.renderCond c
