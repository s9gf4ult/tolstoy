module Tolstoy.DSL.JsonPath.Render
  ( renderQuery
  , renderCondition
  ) where

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
  QueryMethodCall mc q -> mconcat $ interspace
    [ ]

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
renderCondition = (error "FIXME: not implemented")
  -- \case
  -- NotCondition cond -> "!(" <> renderCond cond <> ")"
  -- ExistsCondition cond -> "exists(" <> renderCond cond <> ")"
  -- BoolCondition op a b -> mconcat $ interspace
  --   [ wrapBrackets $ renderCond a
  --   , renderBoolOperator op
  --   , wrapBrackets $ renderCond b ]
  -- EqCondition _ op a b -> mconcat $ interspace
  --   [ wrapBrackets $ renderValue a
  --   , renderEqOperator op
  --   , wrapBrackets $ renderValue b ]
  -- StringCondition v check -> mconcat $ interspace
  --   [ wrapBrackets $ renderValue v
  --   , renderStringChecker check ]
  -- NumberCompareCondition op a b -> mconcat $ interspace
  --   [ wrapBrackets $ renderValue a
  --   , renderNumberCompare op
  --   , wrapBrackets $ renderValue b ]

-- renderStringChecker :: StringChecker -> Builder
-- renderStringChecker = \case
--   StringStartsWith t -> "starts with " <> quoteText t
--   StringLikeRegex r flist -> "like_regex " <> quoteText r <> flags
--     where
--       flags = case flist of
--         [] -> mempty
--         _  -> " flags " <> quoteText flagsText
--       flagsText = T.pack $ renderFlag <$> flist

-- renderFlag :: RegexFlag -> Char
-- renderFlag = \case
--   RegexIFlag -> 'i'
--   RegexMFlag -> 'm'
--   RegexSFlag -> 's'
--   RegexQFlag -> 'q'

-- renderNumberCompare :: NumberCompare -> Builder
-- renderNumberCompare = \case
--   NumberLT -> "<"
--   NumberLE -> "<="
--   NumberGT -> ">"
--   NumberGE -> ">="

-- renderValue :: StructureJsonValue r c t -> Builder
-- renderValue = \case
--   LiteralStringValue t -> quoteText t
--   LiteralNumberValue n -> fromString $ show n
--   LiteralBoolValue b -> case b of
--     True  -> "true"
--     False -> "false"
--   LiteralNullValue -> "null"
--   QueryValue q -> renderQuery q
--   NumberOperatorValue op a b -> mconcat $ interspace
--     [ wrapBrackets $ renderValue a
--     , renderNumberOperator op
--     , wrapBrackets $ renderValue b ]
--   TypeOfValue v ->  wrapBrackets (renderValue v) <> ".type()"
--   SizeOfValue v -> wrapBrackets (renderValue v) <> ".size()"
--   StringToDouble v -> wrapBrackets (renderValue v) <> ".double()"
--   NumberMethodValue m v -> wrapBrackets (renderValue v) <> renderNumberMethod m
--   ObjectAnyFieldValue v -> wrapBrackets (renderValue v) <> ".*"
--   RecursiveElementValue mind v -> wrapBrackets (renderValue v) <> ".**" <> deep
--     where
--       deep :: Builder
--       deep = fold $ mind <&> \ind ->
--         "{" <> renderIndexRange ind <> "}"
--   FilterTypeValue rep val -> mconcat $ interspace
--     [ wrapBrackets $ renderValue val
--     , "?"
--     , wrapBrackets filterRep ]
--     where
--       filterRep = case rep of
--         JsonValueTypeRep n t -> case concat [ nullable, types ] of
--           [cond] -> cond
--           conds  -> mconcat $ L.intersperse " || " $ wrapBrackets <$> conds
--           where
--             nullable = case n of
--               NullableRep -> [ "@ == null" ]
--               StrictRep   -> []
--             types =
--               let
--                 tt = quoteText $ case t of
--                   StringTypeRep  -> "string"
--                   NumberTypeRep  -> "number"
--                   ObjectTypeRep  -> "object"
--                   ArrayTypeRep   -> "array"
--                   NullTypeRep    -> "null"
--                   BooleanTypeRep -> "boolean"
--               in [ "@.type() == " <> tt ]
--   FilterStrictValue v -> wrapBrackets (renderValue v) <> " ? (@ <> null)"

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

renderBoolOperator :: BoolOperator -> Builder
renderBoolOperator = \case
  BoolAnd -> "&&"
  BoolOr  -> "||"

renderEqOperator :: EqOperator -> Builder
renderEqOperator = \case
  EqOperator    -> "=="
  NotEqOperator -> "<>"
