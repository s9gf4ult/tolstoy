module Tolstoy.Structure.Class where

import           Data.Int
import           Data.Proxy
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           GHC.Generics
import           GHC.TypeLits
import           Tolstoy.Structure.Kind
import           Tolstoy.Structure.Value

class Structural s where
  type StructKind s :: Structure
  type StructKind s = GStructKind (Rep s)
  toStructValue :: s -> StructureValue (StructKind s)
  default toStructValue
    :: (Generic s, GStructural (Rep s), StructKind s ~ GStructKind (Rep s))
    => s
    -> StructureValue (StructKind s)
  toStructValue s = gToStructValue (from s)
  fromStructValue :: StructureValue (StructKind s) -> s
  default fromStructValue
    :: (Generic s, GStructural (Rep s), StructKind s ~ GStructKind (Rep s))
    => StructureValue (StructKind s)
    -> s
  fromStructValue s = to (gFromStructValue s)

instance Structural Text where
  type StructKind Text = 'StructString
  toStructValue = StringValue
  fromStructValue (StringValue t) = t

instance Structural Scientific where
  type StructKind Scientific = 'StructNumber
  toStructValue = NumberValue
  fromStructValue (NumberValue s) = s

instance Structural Int where
  type StructKind Int = 'StructNumber
  toStructValue = NumberValue . realToFrac
  fromStructValue (NumberValue v) = fromInteger $ round v

instance Structural Int32 where
  type StructKind Int32 = 'StructNumber
  toStructValue = NumberValue . realToFrac
  fromStructValue (NumberValue v) = fromInteger $ round v

instance Structural Int64 where
  type StructKind Int64 = 'StructNumber
  toStructValue = NumberValue . realToFrac
  fromStructValue (NumberValue v) = fromInteger $ round v

instance Structural Bool where
  type StructKind Bool = 'StructBool
  toStructValue = BoolValue
  fromStructValue (BoolValue b) = b

instance (Structural s) => Structural (Maybe s) where
  type StructKind (Maybe s) = 'StructOptional (StructKind s)
  toStructValue v = OptionalValue $ toStructValue <$> v
  fromStructValue (OptionalValue v) = fromStructValue <$> v

instance (Structural s) => Structural (Vector s) where
  type StructKind (Vector s) = 'StructVector (StructKind s)
  toStructValue v = VectorValue $ toStructValue <$> v
  fromStructValue (VectorValue v) = fromStructValue <$> v

instance (Structural s) => Structural [s] where
  type StructKind [s] = 'StructVector (StructKind s)
  toStructValue l = VectorValue $ toStructValue <$> V.fromList l
  fromStructValue (VectorValue v) = fromStructValue <$> V.toList v

-- | Generic instance deriving
instance (Structural a, Structural b) => Structural (Either a b)

class GStructural (f :: * -> *) where
  type GStructKind f :: Structure
  gToStructValue :: f p -> StructureValue (GStructKind f)
  gFromStructValue :: StructureValue (GStructKind f) -> f p

-- | Top level instance for non-newtypes only
instance
  ( GStructural sub
  ) => GStructural (D1 ('MetaData n m p 'False) sub) where
  type GStructKind (D1 ('MetaData n m p 'False) sub) = GStructKind sub
  gToStructValue (M1 fp) = gToStructValue fp
  gFromStructValue v = M1 $ gFromStructValue v

-- | If constructor uses record syntax, then it is encoded as a tagged
-- product (json object)
instance (GProduct sels) => GStructural (C1 ('MetaCons cn f 'True) sels) where
  type GStructKind (C1 ('MetaCons cn f 'True) sels) =
    'StructProduct (GProdKind sels)
  gToStructValue (M1 fp) = ProductValue (gToProductValue fp)
  gFromStructValue (ProductValue pl) = M1 $ gFromProductValue pl

-- | Constructor not using the record syntax is a sum
instance
  ( GSum (C1 ('MetaCons cn f 'False) sels)
  ) => GStructural (C1 ('MetaCons cn f 'False) sels) where
  type GStructKind (C1 ('MetaCons cn f 'False) sels) =
    'StructSum (GSumKind (C1 ('MetaCons cn f 'False) sels))
  gToStructValue m1 = SumValue (gToSumValue m1)
  gFromStructValue (SumValue pl) = gFromSumValue pl

instance (GSum (l :+: r)) => GStructural (l :+: r) where
  type GStructKind (l :+: r) =
    'StructSum (GSumKind (l :+: r))
  gToStructValue m1 = SumValue (gToSumValue m1)
  gFromStructValue (SumValue pl) = gFromSumValue pl

-- | Encode generic rep as ProductTreeValue
class GProduct (f :: * -> *) where
  type GProdKind f :: ProductTree
  gToProductValue :: f p -> ProductTreeValue (GProdKind f)
  gFromProductValue :: ProductTreeValue (GProdKind f) -> f p

-- | Constructor with no arguments but with record syntax is an empty
-- product
instance GProduct U1 where
  type GProdKind U1 = 'Product0
  gToProductValue U1 = Product0Value
  gFromProductValue Product0Value = U1

-- | Single selector with name is a product of one element
instance
  ( Structural typ
  , KnownSymbol n
  ) => GProduct (S1 ('MetaSel ('Just n) a b c) (Rec0 typ)) where
  type GProdKind (S1 ('MetaSel ('Just n) a b c) (Rec0 typ)) =
    'Product1 n (StructKind typ)
  gToProductValue (M1 (K1 typ)) =
    Product1Value (Proxy @n) (toStructValue typ)
  gFromProductValue (Product1Value _ s) = M1 $ K1 $ fromStructValue s

-- | Product is obviously a product
instance
  ( GProduct l
  , GProduct r
  ) => GProduct (l :*: r) where
  type GProdKind (l :*: r) =
    'Product2 (GProdKind l) (GProdKind r)
  gToProductValue (l :*: r) = Product2Value
    (gToProductValue l) (gToProductValue r)
  gFromProductValue (Product2Value l r) =
    (gFromProductValue l) :*: (gFromProductValue r)

class GSum (f :: * -> *) where
  type GSumKind f :: SumTree
  gToSumValue :: f p -> SumTreeValue (GSumKind f)
  gFromSumValue :: SumTreeValue (GSumKind f) -> f p

-- | Constructor with no arguments is just a single element enum
-- (tagged sum with no data assigned)
instance
  ( KnownSymbol cn
  ) => GSum (C1 ('MetaCons cn f 'False) U1) where
  type GSumKind (C1 ('MetaCons cn f 'False) U1) = 'Sum1 cn StructureEmpty
  gToSumValue (M1 _) = Sum1Value (Proxy @cn) emptyValue
  gFromSumValue (Sum1Value _ _) = M1 U1

-- | Constructor with single argument is a single element tagged sum
instance
  ( KnownSymbol cn
  , Structural typ
  ) => GSum (C1 ('MetaCons cn f 'False) (S1 st (Rec0 typ))) where
  type GSumKind (C1 ('MetaCons cn f 'False) (S1 st (Rec0 typ))) =
    'Sum1 cn (StructKind typ)
  gToSumValue (M1 (M1 (K1 typ))) = Sum1Value (Proxy @cn) (toStructValue typ)
  gFromSumValue (Sum1Value _ s) = M1 $ M1 $ K1 $ fromStructValue s

-- | Sum is obviously a sum
instance
  ( GSum l
  , GSum r
  ) => GSum (l :+: r) where
  type GSumKind (l :+: r) = 'Sum2 (GSumKind l) (GSumKind r)
  gToSumValue = \case
    L1 l -> Sum2Left $ gToSumValue l
    R1 r -> Sum2Right $ gToSumValue r
  gFromSumValue = \case
    Sum2Left l -> L1 $ gFromSumValue l
    Sum2Right r -> R1 $ gFromSumValue r

instance
  ( KnownSymbol cn
  , GProduct sels
  ) => GSum (C1 ('MetaCons cn f 'True) sels) where
  type GSumKind (C1 ('MetaCons cn f 'True) sels) =
    'Sum1 cn ('StructProduct (GProdKind sels))
  gToSumValue (M1 sels) = Sum1Value (Proxy @cn)
    (ProductValue $ gToProductValue sels)
  gFromSumValue (Sum1Value _ (ProductValue sels)) = M1 $ gFromProductValue sels
