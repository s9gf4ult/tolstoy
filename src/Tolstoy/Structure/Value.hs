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
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Instances ()
import           Tolstoy.Structure.Kind

data StructureValue :: Structure -> * where
  StringValue   :: Text -> StructureValue 'StructString
  NumberValue   :: Scientific -> StructureValue 'StructNumber
  BoolValue     :: Bool -> StructureValue 'StructBool
  OptionalValue :: Maybe (StructureValue s) -> StructureValue ('StructOptional s)
  VectorValue   :: Vector (StructureValue s) -> StructureValue ('StructVector s)
  SumValue      :: SumTreeValue t -> StructureValue ('StructSum t)
  ProductValue  :: ProductTreeValue t -> StructureValue ('StructProduct t)

data SumTreeValue :: SumTree -> * where
  Sum1Value
    :: (KnownSymbol n)
    => Proxy n
    -> StructureValue s
    -> SumTreeValue ('Sum1 n s)
  Sum2Left
    :: SumTreeValue t1
    -> SumTreeValue ('Sum2 t1 t2)
  Sum2Right
    :: SumTreeValue t2
    -> SumTreeValue ('Sum2 t1 t2)

data ProductTreeValue :: ProductTree -> * where
  ProductValue0 :: ProductTreeValue 'Product0
  ProductValue1
    :: (KnownSymbol n)
    => Proxy n
    -> StructureValue s
    -> ProductTreeValue ('Product1 n s)
  ProductValue2
    :: ProductTreeValue t1
    -> ProductTreeValue t2
    -> ProductTreeValue ('Product2 t1 t2)

emptyValue :: StructureValue StructureEmpty
emptyValue = ProductValue ProductValue0

-- class FromObject a where
--   parseObject :: Object -> Parser a

-- sumValueJson :: SumValueL l -> [Pair]
-- sumValueJson = \case
--   ThisValue t s ->
--     [ "tag"   .= (T.pack $ symbolVal t)
--     , "value" .= s ]
--   ThatValue that -> sumValueJson that

-- productValueJson :: ProductValueL l -> [Pair]
-- productValueJson = \case
--   ProductNil -> []
--   ProductCons t s rest -> ((T.pack $ symbolVal t) .= s)
--     : productValueJson rest

-- showProxy :: (KnownSymbol s) => Proxy s -> String
-- showProxy p = "(Proxy @\"" ++ symbolVal p ++ "\")"

-- ---------------------------
-- -- StructValue instances --
-- ---------------------------

-- instance ToJSON (StructureValue s) where
--   toJSON = \case
--     StringValue t   -> toJSON t
--     NumberValue s   -> toJSON s
--     BoolValue b     -> toJSON b
--     OptionalValue v -> toJSON v
--     VectorValue v   -> toJSON v
--     SumValue l      -> object $ sumValueJson l
--     ProductValue l  -> object $ productValueJson l

-- instance Show (StructureValue s) where
--   show = \case
--     StringValue t   -> "(StringValue " ++ show t ++ ")"
--     NumberValue s   -> "(NumberValue " ++ show s ++ ")"
--     BoolValue b     -> "(BoolValue " ++ show b ++ ")"
--     OptionalValue v -> "(OptionalValue (" ++ show v ++ "))"
--     VectorValue v   -> "(VectorValue " ++ show v ++ ")"
--     SumValue l      -> "(SumValue " ++ show l ++ ")"
--     ProductValue l  -> "(ProductValue " ++ show l ++ ")"


-- instance Eq (StructureValue 'StructString) where
--   (StringValue a) == (StringValue b) = a == b

-- instance Arbitrary (StructureValue 'StructString) where
--   arbitrary = StringValue <$> arbitrary

-- instance FromJSON (StructureValue 'StructString) where
--   parseJSON v = StringValue <$> parseJSON v

-- instance Eq (StructureValue 'StructNumber) where
--   (NumberValue a) == (NumberValue b) = a == b

-- instance Arbitrary (StructureValue 'StructNumber) where
--   arbitrary = NumberValue <$> arbitrary

-- instance FromJSON (StructureValue StructNumber) where
--   parseJSON v = NumberValue <$> parseJSON v

-- instance Eq (StructureValue 'StructBool) where
--   (BoolValue a) == (BoolValue b) = a == b

-- instance Arbitrary (StructureValue 'StructBool) where
--   arbitrary = BoolValue <$> arbitrary

-- instance FromJSON (StructureValue StructBool) where
--   parseJSON v = BoolValue <$> parseJSON v

-- instance
--   ( Eq (StructureValue s)
--   ) => Eq (StructureValue ('StructOptional s)) where
--   (OptionalValue a) == (OptionalValue b) = a == b

-- instance
--   ( Arbitrary (StructureValue s)
--   ) => Arbitrary (StructureValue ('StructOptional s)) where
--   arbitrary = OptionalValue <$> arbitrary

-- instance
--   ( FromJSON (StructureValue s)
--   ) => FromJSON (StructureValue (StructOptional s)) where
--   parseJSON v = OptionalValue <$> parseJSON v

-- instance
--   ( Eq (StructureValue s)
--   ) => Eq (StructureValue ('StructVector s)) where
--   (VectorValue a) == (VectorValue b) = a == b

-- instance
--   ( Arbitrary (StructureValue s)
--   ) => Arbitrary (StructureValue ('StructVector s)) where
--   arbitrary = VectorValue <$> arbitrary

-- instance
--   ( FromJSON (StructureValue s)
--   ) => FromJSON (StructureValue (StructVector s)) where
--   parseJSON v = VectorValue <$> parseJSON v

-- instance
--   ( Eq (SumValueL l)
--   ) => Eq (StructureValue ('StructSum l)) where
--   (SumValue a) == (SumValue b) = a == b

-- instance
--   ( Arbitrary (SumValueL l)
--   ) => Arbitrary (StructureValue ('StructSum l)) where
--   arbitrary = SumValue <$> arbitrary

-- instance (FromObject (SumValueL l))
--   => FromJSON (StructureValue (StructSum l)) where
--   parseJSON v = SumValue <$> withObject "SumValue" parseObject v

-- instance
--   ( Eq (ProductValueL l)
--   ) => Eq (StructureValue ('StructProduct l)) where
--   (ProductValue a) == (ProductValue b) = a == b

-- instance
--   ( Arbitrary (ProductValueL l)
--   ) => Arbitrary (StructureValue ('StructProduct l)) where
--   arbitrary = ProductValue <$> arbitrary

-- instance (FromObject (ProductValueL l))
--   => FromJSON (StructureValue (StructProduct l)) where
--   parseJSON v = ProductValue <$> withObject "SumValue" parseObject v

-- ------------------------
-- -- SumValueL instance --
-- ------------------------

-- instance FromObject (SumValueL '[]) where
--   parseObject o = do
--     tag <- o .: "tag"
--     fail $ "SumValueL: Unknown tag: " ++ (T.unpack tag)

-- instance
--   ( KnownSymbol t
--   , FromJSON (StructureValue s)
--   , FromObject (SumValueL rest) )
--   => FromObject (SumValueL ( '(t, s)  ': rest )) where
--   parseObject o = do
--     let p = Proxy @t
--     tag <- o .: "tag"
--     if tag == (T.pack $ symbolVal p)
--       then do
--       value <- o .: "value"
--       pure $ ThisValue p value
--       else do
--       that <- parseObject o
--       pure $ ThatValue that

-- instance {-# OVERLAPPABLE #-}
--   ( Eq (StructureValue s)
--   , Eq (SumValueL rest)
--   ) => Eq (SumValueL ( '(t, s) ': rest )) where
--   a' == b' = case (a', b') of
--     (ThisValue _ a, ThisValue _ b) -> a == b
--     (ThatValue a, ThatValue b)     -> a == b
--     _                              -> False

-- instance {-# OVERLAPPING #-}
--   ( Eq (StructureValue s)
--   ) => Eq (SumValueL ( '(t, s) ': '[] )) where
--   a' == b' = case (a', b') of
--     (ThisValue _ a, ThisValue _ b) -> a == b
--     _                              -> error "Eq: Impossible happened"

-- instance Show (SumValueL l) where
--   show = \case
--     ThisValue p s -> "(ThisValue " ++ showProxy p ++ " " ++ show s ++ ")"
--     ThatValue r -> "(ThatValue " ++ show r ++ ")"

-- instance {-# OVERLAPPABLE #-}
--   ( Arbitrary (StructureValue s)
--   , Arbitrary (SumValueL rest)
--   , KnownSymbol t
--   ) => Arbitrary (SumValueL ( '(t, s) ': rest )) where
--   arbitrary = oneof [ this, that ]
--     where
--       this = do
--         s <- arbitrary
--         return $ ThisValue (Proxy @t) s
--       that = do
--         rest <- arbitrary
--         return $ ThatValue rest

-- instance {-# OVERLAPPING #-}
--   ( Arbitrary (StructureValue s)
--   , KnownSymbol t
--   ) => Arbitrary (SumValueL ( '(t, s) ': '[] )) where
--   arbitrary = do
--     s <- arbitrary
--     return $ ThisValue (Proxy @t) s

-- instance (FromObject (SumValueL l)) => FromJSON (SumValueL l) where
--   parseJSON = withObject "SumValueL" parseObject

-- ------------------------------
-- -- ProductValueL instances  --
-- ------------------------------

-- instance FromObject (ProductValueL '[]) where
--   parseObject _ = pure ProductNil

-- instance
--   ( KnownSymbol t
--   , FromJSON (StructureValue s)
--   , FromObject (ProductValueL rest) )
--   => FromObject (ProductValueL ( '(t, s)  ': rest )) where
--   parseObject o = do
--     let p = Proxy @t
--     v <- o .: (T.pack $ symbolVal p)
--     rest <- parseObject o
--     pure $ ProductCons p v rest

-- instance
--   ( Eq (StructureValue s)
--   , Eq (ProductValueL rest)
--   ) => Eq (ProductValueL ( '(t, s) ': rest )) where
--   (ProductCons _ a arest) == (ProductCons _ b brest) = a == b && arest == brest

-- instance Eq (ProductValueL '[]) where
--   ProductNil == ProductNil = True

-- instance Show (ProductValueL l) where
--   show = \case
--     ProductNil -> "ProductNil"
--     ProductCons p s rest -> "(ProductCons " ++ showProxy p
--       ++ " " ++ show s
--       ++ " " ++ show rest
--       ++ ")"

-- instance
--   ( Arbitrary (StructureValue s)
--   , Arbitrary (ProductValueL rest)
--   , KnownSymbol t
--   ) => Arbitrary (ProductValueL ( '(t, s) ': rest )) where
--   arbitrary = do
--     s <- arbitrary
--     rest <- arbitrary
--     return $ ProductCons (Proxy @t) s rest

-- instance Arbitrary (ProductValueL '[]) where
--   arbitrary = pure ProductNil

-- instance (FromObject (ProductValueL l)) => FromJSON (ProductValueL l) where
--   parseJSON = withObject "ProductValueL" parseObject
