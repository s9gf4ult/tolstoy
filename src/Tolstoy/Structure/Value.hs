{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Tolstoy.Structure.Value where

import           Data.Aeson
import           Data.Aeson.Types (Pair, Parser)
import           Data.Proxy
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (Vector)
import           GHC.TypeLits
import           Tolstoy.Structure.Kind


data StructureValue :: Structure -> * where
  StringValue   :: Text -> StructureValue StructString
  NumberValue   :: Scientific -> StructureValue StructNumber
  BoolValue     :: Bool -> StructureValue StructBool
  OptionalValue :: Maybe (StructureValue s) -> StructureValue (StructOptional s)
  VectorValue   :: Vector (StructureValue s) -> StructureValue (StructVector s)
  SumValue      :: SumValueL l -> StructureValue (StructSum l)
  ProductValue  :: ProductValueL l -> StructureValue (StructProduct l)

data SumValueL :: [(Symbol, Structure)] -> * where
  ThisValue
    :: forall t s rest
    .  (KnownSymbol t)
    => Proxy t
    -> StructureValue s
    -> SumValueL ('(t, s) ': rest)
  ThatValue
    :: forall h rest
    .  SumValueL rest
    -> SumValueL (h ': rest)

data ProductValueL :: [(Symbol, Structure)] -> * where
  ProductNil :: ProductValueL '[]
  ProductCons
    :: (KnownSymbol t)
    => Proxy t
    -> StructureValue s
    -> ProductValueL rest
    -> ProductValueL ('(t, s) ': rest)

structEmpty :: StructureValue StructEmpty
structEmpty = ProductValue ProductNil

instance Eq (StructureValue 'StructString) where
  (StringValue a) == (StringValue b) = a == b

instance Show (StructureValue 'StructString) where
  show (StringValue s) = "(StringValue " ++ show s ++ ")"

instance Eq (StructureValue 'StructNumber) where
  (NumberValue a) == (NumberValue b) = a == b

instance Show (StructureValue 'StructNumber) where
  show (NumberValue s) = "(NumberValue " ++ show s ++ ")"

instance Eq (StructureValue 'StructBool) where
  (BoolValue a) == (BoolValue b) = a == b

instance Show (StructureValue 'StructBool) where
  show (BoolValue s) = "(BoolValue " ++ show s ++ ")"

instance
  ( Eq (StructureValue s)
  ) => Eq (StructureValue ('StructOptional s)) where
  (OptionalValue a) == (OptionalValue b) = a == b

instance
  ( Show (StructureValue s)
  ) => Show (StructureValue ('StructOptional s)) where
  show (OptionalValue s) = "(OptionalValue (" ++ show s ++ "))"

instance
  ( Eq (StructureValue s)
  ) => Eq (StructureValue ('StructVector s)) where
  (VectorValue a) == (VectorValue b) = a == b

instance
  ( Show (StructureValue s)
  ) => Show (StructureValue (StructVector s)) where
  show (VectorValue s) = "(VectorValue " ++ show s ++ ")"

instance ToJSON (StructureValue s) where
  toJSON = \case
    StringValue t   -> toJSON t
    NumberValue s   -> toJSON s
    BoolValue b     -> toJSON b
    OptionalValue v -> toJSON v
    VectorValue v   -> toJSON v
    SumValue l      -> object $ sumValueJson l
    ProductValue l  -> object $ productValueJson l

sumValueJson :: SumValueL l -> [Pair]
sumValueJson = \case
  ThisValue t s ->
    [ "tag"   .= (T.pack $ symbolVal t)
    , "value" .= s ]
  ThatValue that -> sumValueJson that

productValueJson :: ProductValueL l -> [Pair]
productValueJson = \case
  ProductNil -> []
  ProductCons t s rest -> ((T.pack $ symbolVal t) .= s)
    : productValueJson rest

instance FromJSON (StructureValue 'StructString) where
  parseJSON v = StringValue <$> parseJSON v

instance FromJSON (StructureValue StructNumber) where
  parseJSON v = NumberValue <$> parseJSON v

instance FromJSON (StructureValue StructBool) where
  parseJSON v = BoolValue <$> parseJSON v

instance (FromJSON (StructureValue s))
  => FromJSON (StructureValue (StructOptional s)) where
  parseJSON v = OptionalValue <$> parseJSON v

instance (FromJSON (StructureValue s))
  => FromJSON (StructureValue (StructVector s)) where
  parseJSON v = VectorValue <$> parseJSON v

instance (FromObject (SumValueL l))
  => FromJSON (StructureValue (StructSum l)) where
  parseJSON v = SumValue <$> withObject "SumValue" parseObject v

instance (FromObject (ProductValueL l))
  => FromJSON (StructureValue (StructProduct l)) where
  parseJSON v = ProductValue <$> withObject "SumValue" parseObject v

class FromObject a where
  parseObject :: Object -> Parser a

instance FromObject (ProductValueL '[]) where
  parseObject _ = pure ProductNil

instance
  ( KnownSymbol t
  , FromJSON (StructureValue s)
  , FromObject (ProductValueL rest) )
  => FromObject (ProductValueL ( '(t, s)  ': rest )) where
  parseObject o = do
    let p = Proxy @t
    v <- o .: (T.pack $ symbolVal p)
    rest <- parseObject o
    pure $ ProductCons p v rest

instance
  ( KnownSymbol t
  , FromJSON (StructureValue s)
  , FromObject (SumValueL rest) )
  => FromObject (SumValueL ( '(t, s)  ': rest )) where
  parseObject o = do
    let p = Proxy @t
    tag <- o .: "tag"
    if tag == (T.pack $ symbolVal p)
      then do
      value <- o .: "value"
      pure $ ThisValue p value
      else do
      that <- parseObject o
      pure $ ThatValue that

instance FromObject (SumValueL '[]) where
  parseObject o = fail "Tag not found"
