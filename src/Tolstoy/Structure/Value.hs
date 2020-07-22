module Tolstoy.Structure.Value where


import           Data.Aeson (FromJSON(..), ToJSON(..), Value, (.:), (.=))
import qualified Data.Aeson as J
import           Data.Aeson.Types (Pair, Parser)
import           Data.Functor ((<$>))
import           Data.Maybe
import           Data.Proxy
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           GHC.TypeLits
import           Prelude (error, fail, mconcat, pure, ($), (++), (==))
import qualified Prelude as P
import           Tolstoy.Structure.Kind

data StructureValue :: Structure -> * where
  StringValue   :: Text -> StructureValue StructString
  NumberValue   :: Scientific -> StructureValue StructNumber
  BoolValue     :: P.Bool -> StructureValue StructBool
  NullValue     :: StructureValue StructNull
  OptionalValue :: Maybe (StructureValue s) -> StructureValue (StructOptional s)
  VectorValue   :: V.Vector (StructureValue s) -> StructureValue (StructVector s)
  SumValue      :: SumValueL l -> StructureValue (StructSum l)
  ProductValue  :: ProductValueL l -> StructureValue (StructProduct l)

data SumValueL :: [(Symbol, Structure)] -> * where
  ThisValue
    :: forall t s tail
    .  (KnownSymbol t)
    => Proxy t
    -> StructureValue s
    -> SumValueL ('(t, s) ': tail)
  ThatValue
    :: forall h tail
    .  SumValueL tail
    -> SumValueL (h ': tail)

data ProductValueL :: [(Symbol, Structure)] -> * where
  ProductNil :: ProductValueL '[]
  ProductCons
    :: (KnownSymbol t)
    => Proxy t
    -> StructureValue s
    -> ProductValueL tail
    -> ProductValueL ('(t, s) ': tail)

instance ToJSON (StructureValue s) where
  toJSON = \case
    StringValue t   -> toJSON t
    NumberValue s   -> toJSON s
    BoolValue b     -> toJSON b
    NullValue       -> J.Null
    OptionalValue v -> toJSON v
    VectorValue v   -> toJSON v
    SumValue l      -> J.object $ sumValueJson l
    ProductValue l  -> J.object $ productValueJson l

sumValueJson :: SumValueL l -> [Pair]
sumValueJson = \case
  ThisValue t s ->
    [ "tag"   .= (T.pack $ symbolVal t)
    , "value" .= s ]
  ThatValue that -> sumValueJson that

productValueJson :: ProductValueL l -> [Pair]
productValueJson = \case
  ProductNil -> []
  ProductCons t s tail -> ((T.pack $ symbolVal t) .= s)
    : productValueJson tail

instance FromJSON (StructureValue 'StructString) where
  parseJSON v = StringValue <$> parseJSON v

instance FromJSON (StructureValue StructNumber) where
  parseJSON v = NumberValue <$> parseJSON v

instance FromJSON (StructureValue StructBool) where
  parseJSON v = BoolValue <$> parseJSON v

instance FromJSON (StructureValue StructNull) where
  parseJSON = \case
    J.Null -> pure NullValue
    _ -> fail "Null expected"

instance (FromJSON (StructureValue s))
  => FromJSON (StructureValue (StructOptional s)) where
  parseJSON v = OptionalValue <$> parseJSON v

instance (FromJSON (StructureValue s))
  => FromJSON (StructureValue (StructVector s)) where
  parseJSON v = VectorValue <$> parseJSON v

instance (FromObject (SumValueL l))
  => FromJSON (StructureValue (StructSum l)) where
  parseJSON v = SumValue <$> J.withObject "SumValue" parseObject v

instance (FromObject (ProductValueL l))
  => FromJSON (StructureValue (StructProduct l)) where
  parseJSON v = ProductValue <$> J.withObject "SumValue" parseObject v

class FromObject a where
  parseObject :: J.Object -> Parser a

instance FromObject (ProductValueL '[]) where
  parseObject _ = pure ProductNil

instance
  ( KnownSymbol t
  , FromJSON (StructureValue s)
  , FromObject (ProductValueL tail) )
  => FromObject (ProductValueL ( '(t, s)  ': tail )) where
  parseObject o = do
    let p = Proxy @t
    v <- o .: (T.pack $ symbolVal p)
    tail <- parseObject o
    pure $ ProductCons p v tail

instance
  ( KnownSymbol t
  , FromJSON (StructureValue s)
  , FromObject (SumValueL tail) )
  => FromObject (SumValueL ( '(t, s)  ': tail )) where
  parseObject o = do
    let p = Proxy @t
    tag <- o .: "tag"
    if tag == (T.pack $ symbolVal p)
      then do
      value <- o .: "value"
      pure $ ThisValue p value
      else do
      parseObject o

instance FromObject (SumValueL '[]) where
  parseObject o = fail "Tag not found"
