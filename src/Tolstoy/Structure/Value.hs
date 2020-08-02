{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Tolstoy.Structure.Value where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types (Pair, Parser)
import           Data.Proxy
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (Vector)
import           GHC.TypeLits
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Instances ()
import           Tolstoy.Structure.Kind

data StructureValue :: Structure -> * where
  StringValue   :: !Text -> StructureValue 'StructString
  NumberValue   :: !Scientific -> StructureValue 'StructNumber
  BoolValue     :: !Bool -> StructureValue 'StructBool
  OptionalValue
    :: !(Maybe (StructureValue s))
    -> StructureValue ('StructOptional s)
  VectorValue
    :: !(Vector (StructureValue s))
    -> StructureValue ('StructVector s)
  SumValue
    :: !(SumTreeValue t)
    -> StructureValue ('StructSum t)
  ProductValue
    :: !(ProductTreeValue t)
    -> StructureValue ('StructProduct t)

data SumTreeValue :: SumTree -> * where
  Sum1Value
    :: (KnownSymbol n)
    => Proxy n
    -> !(StructureValue s)
    -> SumTreeValue ('Sum1 n s)
  Sum2Left
    :: !(SumTreeValue t1)
    -> SumTreeValue ('Sum2 t1 t2)
  Sum2Right
    :: !(SumTreeValue t2)
    -> SumTreeValue ('Sum2 t1 t2)

data ProductTreeValue :: ProductTree -> * where
  Product0Value :: ProductTreeValue 'Product0
  Product1Value
    :: (KnownSymbol n)
    => Proxy n
    -> !(StructureValue s)
    -> ProductTreeValue ('Product1 n s)
  Product2Value
    :: !(ProductTreeValue t1)
    -> !(ProductTreeValue t2)
    -> ProductTreeValue ('Product2 t1 t2)

emptyValue :: StructureValue StructureEmpty
emptyValue = ProductValue Product0Value

class FromObject a where
  parseObject :: Object -> Parser a

showProxy :: (KnownSymbol s) => Proxy s -> String
showProxy p = "(Proxy @\"" ++ symbolVal p ++ "\")"

proxyText :: (KnownSymbol s) => Proxy s -> Text
proxyText p = T.pack $ symbolVal p

sumValueJson :: SumTreeValue a -> [Pair]
sumValueJson = \case
  Sum1Value t s ->
    [ "tag"   .= proxyText t
    , "value" .= s ]
  Sum2Left l -> sumValueJson l
  Sum2Right r -> sumValueJson r

productValueJson :: ProductTreeValue a -> [Pair]
productValueJson = \case
  Product0Value -> []
  Product1Value t s -> pure $ (proxyText t) .= s
  Product2Value l r -> productValueJson l ++ productValueJson r


-- ---------------------------
-- -- StructValue instances --
-- ---------------------------

instance ToJSON (StructureValue s) where
  toJSON = \case
    StringValue t   -> toJSON t
    NumberValue s   -> toJSON s
    BoolValue b     -> toJSON b
    OptionalValue v -> toJSON v
    VectorValue v   -> toJSON v
    SumValue l      -> object $ sumValueJson l
    ProductValue l  -> object $ productValueJson l

instance Show (StructureValue s) where
  show = \case
    StringValue t   -> "(StringValue " ++ show t ++ ")"
    NumberValue s   -> "(NumberValue " ++ show s ++ ")"
    BoolValue b     -> "(BoolValue " ++ show b ++ ")"
    OptionalValue v -> "(OptionalValue (" ++ show v ++ "))"
    VectorValue v   -> "(VectorValue " ++ show v ++ ")"
    SumValue l      -> "(SumValue " ++ show l ++ ")"
    ProductValue l  -> "(ProductValue " ++ show l ++ ")"


instance Eq (StructureValue 'StructString) where
  (StringValue a) == (StringValue b) = a == b

instance Arbitrary (StructureValue 'StructString) where
  arbitrary = StringValue <$> arbitrary

instance FromJSON (StructureValue 'StructString) where
  parseJSON v = StringValue <$> parseJSON v

instance Eq (StructureValue 'StructNumber) where
  (NumberValue a) == (NumberValue b) = a == b

instance Arbitrary (StructureValue 'StructNumber) where
  arbitrary = NumberValue <$> arbitrary

instance FromJSON (StructureValue StructNumber) where
  parseJSON v = NumberValue <$> parseJSON v

instance Eq (StructureValue 'StructBool) where
  (BoolValue a) == (BoolValue b) = a == b

instance Arbitrary (StructureValue 'StructBool) where
  arbitrary = BoolValue <$> arbitrary

instance FromJSON (StructureValue StructBool) where
  parseJSON v = BoolValue <$> parseJSON v

instance
  ( Eq (StructureValue s)
  ) => Eq (StructureValue ('StructOptional s)) where
  (OptionalValue a) == (OptionalValue b) = a == b

instance
  ( Arbitrary (StructureValue s)
  ) => Arbitrary (StructureValue ('StructOptional s)) where
  arbitrary = OptionalValue <$> arbitrary

instance
  ( FromJSON (StructureValue s)
  ) => FromJSON (StructureValue (StructOptional s)) where
  parseJSON v = OptionalValue <$> parseJSON v

instance
  ( Eq (StructureValue s)
  ) => Eq (StructureValue ('StructVector s)) where
  (VectorValue a) == (VectorValue b) = a == b

instance
  ( Arbitrary (StructureValue s)
  ) => Arbitrary (StructureValue ('StructVector s)) where
  arbitrary = VectorValue <$> arbitrary

instance
  ( FromJSON (StructureValue s)
  ) => FromJSON (StructureValue (StructVector s)) where
  parseJSON v = VectorValue <$> parseJSON v

instance
  ( Eq (SumTreeValue l)
  ) => Eq (StructureValue ('StructSum l)) where
  (SumValue a) == (SumValue b) = a == b

instance
  ( Arbitrary (SumTreeValue l)
  ) => Arbitrary (StructureValue ('StructSum l)) where
  arbitrary = SumValue <$> arbitrary

instance (FromObject (SumTreeValue t))
  => FromJSON (StructureValue (StructSum t)) where
  parseJSON v = SumValue <$> withObject "SumValue" parseObject v

instance
  ( Eq (ProductTreeValue l)
  ) => Eq (StructureValue ('StructProduct l)) where
  (ProductValue a) == (ProductValue b) = a == b

instance
  ( Arbitrary (ProductTreeValue l)
  ) => Arbitrary (StructureValue ('StructProduct l)) where
  arbitrary = ProductValue <$> arbitrary

instance (FromObject (ProductTreeValue t))
  => FromJSON (StructureValue (StructProduct t)) where
  parseJSON v = ProductValue <$> withObject "ProductValue" parseObject v


-----------------------------
-- SumTreeValue instances  --
-----------------------------


instance
  ( KnownSymbol t
  , FromJSON (StructureValue s)
  ) => FromObject (SumTreeValue ('Sum1 t s)) where
  parseObject o = do
    let p = Proxy @t
    tag <- o .: "tag"
    if tag == (proxyText p)
      then do
      value <- o .: "value"
      pure $ Sum1Value p value
      else do
      fail $ "SumTreeValue: Unknown tag: " ++ (T.unpack tag)

instance
  ( FromObject (SumTreeValue l)
  , FromObject (SumTreeValue r)
  ) => FromObject (SumTreeValue ('Sum2 l r)) where
  parseObject o
    =   (Sum2Left <$> parseObject o)
    <|> (Sum2Right <$> parseObject o)

instance
  ( Eq (StructureValue s)
  ) => Eq (SumTreeValue ('Sum1 n s)) where
  (Sum1Value _ a) == (Sum1Value _ b) = a == b

instance
  ( Eq (SumTreeValue t1)
  , Eq (SumTreeValue t2)
  ) => Eq (SumTreeValue ('Sum2 t1 t2)) where
  a' == b' = case (a', b') of
    (Sum2Left a, Sum2Left b)   -> a == b
    (Sum2Right a, Sum2Right b) -> a == b
    _                          -> False


instance Show (SumTreeValue t) where
  show = \case
    Sum1Value p s -> "(Sum1Value " ++ showProxy p ++ " " ++ show s ++ ")"
    Sum2Left l    -> "(Sum2Left " ++ show l ++ ")"
    Sum2Right r   -> "(Sum2Right " ++ show r ++ ")"

instance
  ( Arbitrary (StructureValue s)
  , KnownSymbol n
  ) => Arbitrary (SumTreeValue ('Sum1 n s)) where
  arbitrary = Sum1Value Proxy <$> arbitrary

instance
  ( Arbitrary (SumTreeValue t1)
  , Arbitrary (SumTreeValue t2)
  ) => Arbitrary (SumTreeValue ('Sum2 t1 t2)) where
  arbitrary = oneof
    [ Sum2Left <$> arbitrary
    , Sum2Right <$> arbitrary ]

instance (FromObject (SumTreeValue l)) => FromJSON (SumTreeValue l) where
  parseJSON = withObject "SumTreeValue" parseObject

---------------------------------
-- ProductTreeValue instances  --
---------------------------------

instance FromObject (ProductTreeValue 'Product0) where
  parseObject _ = pure Product0Value

instance
  ( KnownSymbol t
  , FromJSON (StructureValue s)
  ) => FromObject (ProductTreeValue ('Product1 t s)) where
  parseObject o = do
    let p = Proxy @t
    v <- o .: proxyText p
    pure $ Product1Value p v

instance
  ( FromObject (ProductTreeValue t1)
  , FromObject (ProductTreeValue t2)
  ) => FromObject (ProductTreeValue ('Product2 t1 t2)) where
  parseObject o = do
    t1 <- parseObject o
    t2 <- parseObject o
    pure $ Product2Value t1 t2

instance Eq (ProductTreeValue 'Product0) where
  Product0Value == Product0Value = True

instance
  ( Eq (StructureValue s)
  ) => Eq (ProductTreeValue ('Product1 n s)) where
  (Product1Value _ a) == (Product1Value _ b) = a == b

instance
  ( Eq (ProductTreeValue t1)
  , Eq (ProductTreeValue t2)
  ) => Eq (ProductTreeValue ('Product2 t1 t2)) where
  (Product2Value a aa) == (Product2Value b bb) = a == b && aa == bb


instance Show (ProductTreeValue l) where
  show = \case
    Product0Value -> "Product0Value"
    Product1Value p s -> "(Product1Value "  ++ showProxy p
      ++ " " ++ show s ++ ")"
    Product2Value t1 t2 -> "(Product2Value "
      ++ show t1 ++ " "
      ++ show t2 ++ ")"

instance Arbitrary (ProductTreeValue 'Product0) where
  arbitrary = pure Product0Value

instance
  ( Arbitrary (StructureValue s)
  , KnownSymbol n
  ) => Arbitrary (ProductTreeValue ('Product1 n s)) where
  arbitrary = Product1Value Proxy <$> arbitrary

instance
  ( Arbitrary (ProductTreeValue t1)
  , Arbitrary (ProductTreeValue t2)
  ) => Arbitrary (ProductTreeValue ('Product2 t1 t2)) where
  arbitrary = Product2Value <$> arbitrary <*> arbitrary

instance (FromObject (ProductTreeValue l)) => FromJSON (ProductTreeValue l) where
  parseJSON = withObject "ProductTreeValue" parseObject
