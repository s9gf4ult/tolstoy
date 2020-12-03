module Tolstoy.DB.JsonPath where

import qualified Data.List as L
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB
import           GHC.TypeLits
import           Tolstoy.Structure.JsonPath

renderQuery :: StructureQuery r c ret -> Builder
renderQuery = \case
  QueryRoot -> "$"
  QueryContext -> "@"
  QueryFilter q cond -> mconcat $ interspace $
    [ renderQuery q
    , "? ("
    , renderCond cond
    , ")" ]
  QueryNesting q p -> renderQuery q <> renderJsonPath p

renderJsonPath :: StructurePath s1 s2 -> Builder
renderJsonPath = \case
  OptionalPath -> " ? (@ <> null)"
  VectorPath vind -> "[" <> renderVectorIndex vind <> "]"
  SumPath sumPath -> renderSumPath sumPath
  ProductPath prodPath -> renderProductPath prodPath

renderVectorIndex :: VectorIndex -> Builder
renderVectorIndex = \case
  VectorAny     -> "*"
  VectorRange r -> case r of
    IndexExact v   -> fromString $ show v
    IndexRange a b -> fromString $ show a <> "-" <> show b

quoteText :: Text -> Builder
quoteText = error "FIXME: quoteText not implemented"

wrapBrackets :: Builder -> Builder
wrapBrackets a = "(" <> a <> ")"

interspace :: [Builder] -> [Builder]
interspace = L.intersperse $ TB.singleton ' '

renderSumPath :: SumPathTree s sub -> Builder
renderSumPath = \case
  Sum1PathTree n -> " ? (@.tag == " <> quoteText tagName <> ").value"
    where
      tagName = T.pack $ symbolVal n
  Sum2LeftPathTree l -> renderSumPath l
  Sum2RightPathTree r -> renderSumPath r

renderProductPath :: ProductPathTree s sub -> Builder
renderProductPath = \case
  Product1PathTree n -> "." <> quoteText tagName
    where
      tagName = T.pack $ symbolVal n
  Product2LeftPathTree l -> renderProductPath l
  Product2RightPathTree r -> renderProductPath r

renderCond :: StructureCondition r c -> Builder
renderCond = \case
  NotCondition cond -> "!(" <> renderCond cond <> ")"
  ExistsCondition cond -> "exists(" <> renderCond cond <> ")"
  BoolCondition op a b -> mconcat $ interspace
    [ wrapBrackets $ renderCond a
    , renderBoolOperator op
    , wrapBrackets $ renderCond b ]
  EqCondition _ op a b -> mconcat $ interspace
    [ wrapBrackets $ renderValue a
    , renderEqOperator op
    , wrapBrackets $ renderValue b ]
  EqLaxCondition _ op a b -> mconcat $ interspace
    [ wrapBrackets $ renderValue a
    , renderEqOperator op
    , wrapBrackets $ renderValue b ]
  StringCondition v check -> mconcat $ interspace
    [ wrapBrackets $ renderValue v
    , renderStringChecker check ]
  NumberCompareCondition op a b -> mconcat $ interspace
    [ wrapBrackets $ renderValue a
    , renderNumberCompare op
    , wrapBrackets $ renderValue b ]

renderStringChecker :: StringChecker -> Builder
renderStringChecker = \case
  StringStartsWith t -> "starts with " <> quoteText t
  StringLikeRegex r flist -> "like_regex " <> quoteText r <> flags
    where
      flags = case flist of
        [] -> mempty
        _  -> " flags " <> quoteText flagsText
      flagsText = T.pack $ renderFlag <$> flist

renderFlag :: RegexFlag -> Char
renderFlag = \case
  RegexIFlag -> 'i'
  RegexMFlag -> 'm'
  RegexSFlag -> 's'
  RegexQFlag -> 'q'

renderNumberCompare :: NumberCompare -> Builder
renderNumberCompare = \case
  NumberLT -> "<"
  NumberLE -> "<="
  NumberGT -> ">"
  NumberGE -> ">="

renderValue :: StructureJsonValue r c t -> Builder
renderValue = \case
  LiteralStringValue t -> quoteText t
  LiteralNumberValue n -> fromString $ show n
  LiteralBoolValue b -> case b of
    True  -> "true"
    False -> "false"
  LiteralNullValue -> "null"
  QueryValue q -> renderQuery q

-- | The root of the path
data Root = Document | Context

renderRoot :: Root -> Builder
renderRoot = \case
  Document -> "$"
  Context -> "@"

renderBoolOperator :: BoolOperator -> Builder
renderBoolOperator = \case
  BoolAnd -> "&&"
  BoolOr  -> "||"

renderEqOperator :: EqOperator -> Builder
renderEqOperator = \case
  EqOperator    -> "=="
  NotEqOperator -> "<>"
