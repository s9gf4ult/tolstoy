module Tolstoy.DSL.JsonPath.Render
  ( renderQuery
  , renderCondition
  ) where

import qualified Data.List as L
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB
import           GHC.TypeLits
import           Tolstoy.Structure.JsonPath
import           Tolstoy.Structure.Rep

renderQuery :: StructureQuery r c ret -> Builder
renderQuery = \case
  QueryRoot -> "$"
  QueryContext -> "@"
  QueryFilter cond q -> mconcat $ interspace
    [ renderQuery q
    , "?"
    , wrapBrackets $ renderCondition cond ]
  QueryPath p q -> renderQuery q <> renderJsonPath p
  QueryLiteral lit -> renderLiteral lit
  QueryNumberOperator op a b -> mconcat $ interspace
    [ renderQuery a
    , renderNumberOperator op
    , renderQuery b
    ]
  QueryMethodCall mc q -> renderQuery q <> renderMethodCall mc
  QueryAnyField rep _af q -> mconcat
    [ renderQuery q
    , ".*"
    , renderTypeFilter rep ]
  QueryRecursiveAnyField rep idx q -> mconcat
    [ renderQuery q
    , ".**"
    , maybe mempty renderRecursiveDepth idx
    , renderTypeFilter rep ]

renderTypeFilter :: StructureRep a -> Builder
renderTypeFilter rep = " ? " <> wrapBrackets (typeFilter rep)
  where
    typeFilter = \case
      OptionalRep sub -> "@ == null || " <> typeFilterStrict sub
      other -> typeFilterStrict other
    typeFilterStrict :: StructureRep x -> Builder
    typeFilterStrict = \case
      OptionalRep sub -> typeFilterStrict sub
      StringRep -> typeIs "string"
      NumberRep -> typeIs "number"
      BoolRep -> typeIs "boolean"
      NullRep -> typeIs "null"
      VectorRep _ -> typeIs "array"
      SumRep _ -> typeIs "object"
      ProductRep _ -> typeIs "object"
    typeIs t = "@.type() == " <> quoteText t

renderRecursiveDepth :: IndexRange -> Builder
renderRecursiveDepth ind = "{" <> renderIndexRange ind <> "}"

renderLiteral :: Literal s -> Builder
renderLiteral = \case
  LiteralString t -> quoteText t
  LiteralNumber num -> fromString $ show num
  LiteralBool b -> case b of
    True  -> "true"
    False -> "false"
  LiteralNull -> "null"

renderJsonPath :: StructurePath s1 s2 -> Builder
renderJsonPath = \case
  OptionalPath -> " ? (@ <> null)"
  VectorPath vind -> "[" <> renderVectorIndex vind <> "]"
  SumPath tag sumPath -> renderSumPath tagName sumPath
    where
      tagName = T.pack $ symbolVal tag
  ProductPath tag prodPath -> renderProductPath tagName prodPath
    where
      tagName = T.pack $ symbolVal tag

renderVectorIndex :: VectorIndex -> Builder
renderVectorIndex = \case
  VectorAny     -> "*"
  VectorRange r -> renderIndexRange r

renderIndexRange :: IndexRange -> Builder
renderIndexRange = \case
  IndexExact v   -> fromString $ show v
  IndexRange a b -> fromString $ show a <> " to " <> show b

quoteText :: Text -> Builder
quoteText t = TB.singleton '"' <> b <> TB.singleton '"'
  where
    b = fromString $ T.unpack t >>= f
    f = \case
      '"' -> "\""
      c   -> [c]

interspace :: [Builder] -> [Builder]
interspace = L.intersperse $ TB.singleton ' '

wrapBrackets :: Builder -> Builder
wrapBrackets a = "(" <> a <> ")"

renderSumPath :: Text -> SumPathTree tag s sub -> Builder
renderSumPath tagName = \case
  Sum1PathTree -> " ? (@.tag == " <> quoteText tagName <> ").value"
  Sum2LeftPathTree l -> renderSumPath tagName l
  Sum2RightPathTree r -> renderSumPath tagName r

renderProductPath :: Text -> ProductPathTree tag s sub -> Builder
renderProductPath tagName = \case
  Product1PathTree -> "." <> quoteText tagName
  Product2LeftPathTree l -> renderProductPath tagName l
  Product2RightPathTree r -> renderProductPath tagName r

renderCondition :: StructureCondition r c -> Builder
renderCondition = \case
  NotCondition cond -> "!(" <> renderCondition cond <> ")"
  ExistsCondition cond -> "exists(" <> renderCondition cond <> ")"
  BoolCondition op a b -> mconcat $ interspace
    [ wrapBrackets $ renderCondition a
    , renderBoolOperator op
    , wrapBrackets $ renderCondition b ]
  EqCondition _ op a b -> mconcat $ interspace
    [ renderQuery a
    , renderEqOperator op
    , renderQuery b ]
  StringCondition v check -> mconcat $ interspace
    [ renderQuery v
    , renderStringChecker check ]
  NumberCompareCondition op a b -> mconcat $ interspace
    [ wrapBrackets $ renderQuery a
    , renderNumberCompare op
    , wrapBrackets $ renderQuery b ]

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

renderNumberOperator :: NumberOperator -> Builder
renderNumberOperator = \case
  NumberPlus -> "+"
  NumberMinus -> "-"
  NumberMultiply -> "*"
  NumberDivide -> "/"
  NumberModulus -> "%"

renderMethodCall :: MethodCall a b -> Builder
renderMethodCall = \case
  CallType -> ".type()"
  CallSize -> ".size()"
  CallDouble _dc -> ".double()"
  CallNumberMethod nm -> renderNumberMethod nm

renderNumberMethod :: NumberMethod -> Builder
renderNumberMethod = \case
  NumberCeiling -> ".ceiling()"
  NumberFloor -> ".floor()"
  NumberAbs -> ".abs()"

renderBoolOperator :: BoolOperator -> Builder
renderBoolOperator = \case
  BoolAnd -> "&&"
  BoolOr  -> "||"

renderEqOperator :: EqOperator -> Builder
renderEqOperator = \case
  EqOperator    -> "=="
  NotEqOperator -> "<>"
