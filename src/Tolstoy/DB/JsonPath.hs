module Tolstoy.DB.JsonPath where

import           Data.Foldable
import           Data.Functor
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
    , "?"
    , wrapBrackets $ renderCond cond ]
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
  VectorRange r -> renderIndexRange r

renderIndexRange :: IndexRange -> Builder
renderIndexRange = \case
  IndexExact v   -> fromString $ show v
  IndexRange a b -> fromString $ show a <> " to " <> show b

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
  NumberOperatorValue op a b -> mconcat $ interspace
    [ wrapBrackets $ renderValue a
    , renderNumberOperator op
    , wrapBrackets $ renderValue b ]
  TypeOfValue v ->  wrapBrackets (renderValue v) <> ".type()"
  SizeOfValue v -> wrapBrackets (renderValue v) <> ".size()"
  StringToDouble v -> wrapBrackets (renderValue v) <> ".double()"
  NumberMethodValue m v -> wrapBrackets (renderValue v) <> renderNumberMethod m
  ObjectAnyFieldValue v -> wrapBrackets (renderValue v) <> ".*"
  RecursiveElementValue mind v -> wrapBrackets (renderValue v) <> ".**" <> deep
    where
      deep :: Builder
      deep = fold $ mind <&> \ind ->
        "{" <> renderIndexRange ind <> "}"
  FilterTypeValue rep val -> mconcat $ interspace
    [ wrapBrackets $ renderValue val
    , "?"
    , wrapBrackets filterRep ]
    where
      filterRep = case rep of
        JsonValueTypeRep n t -> case concat [ nullable, types ] of
          [cond] -> cond
          conds  -> mconcat $ L.intersperse " || " $ wrapBrackets <$> conds
          where
            nullable = case n of
              NullableRep -> [ "@ == null" ]
              StrictRep   -> []
            types =
              let
                tt = quoteText $ case t of
                  StringTypeRep  -> "string"
                  NumberTypeRep  -> "number"
                  ObjectTypeRep  -> "object"
                  ArrayTypeRep   -> "array"
                  NullTypeRep    -> "null"
                  BooleanTypeRep -> "boolean"
              in [ "@.type() == " <> tt ]
  FilterStrictValue v -> wrapBrackets (renderValue v) <> " ? (@ <> null)"

renderNumberOperator :: NumberOperator -> Builder
renderNumberOperator = \case
  NumberPlus -> "+"
  NumberMinus -> "-"
  NumberMultiply -> "*"
  NumberDivide -> "/"
  NumberModulus -> "%"

renderNumberMethod :: NumberMethod -> Builder
renderNumberMethod = \case
  NumberCeiling -> ".ceiling()"
  NumberFloor -> ".floor()"
  NumberAbs -> ".abs()"
  NumberDouble -> ".double()"

renderBoolOperator :: BoolOperator -> Builder
renderBoolOperator = \case
  BoolAnd -> "&&"
  BoolOr  -> "||"

renderEqOperator :: EqOperator -> Builder
renderEqOperator = \case
  EqOperator    -> "=="
  NotEqOperator -> "<>"
