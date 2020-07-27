module Tolstoy.Structure.Class where

import Data.Proxy
import Data.Scientific
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics
import GHC.TypeLits
import Tolstoy.Structure.Kind
import Tolstoy.Structure.Value

class Structural s where
  type StructKind s :: Structure
  -- type StructKind s = GStructKind (Rep s)
  toStructValue :: s -> StructureValue (StructKind s)
  -- default toStructValue
  --   :: (Generic s, GStructural (Rep s))
  --   => s
  --   -> StructureValue (StructKind s)
  -- toStructValue s = gToStructValue (from s)
  fromStructValue :: StructureValue (StructKind s) -> s

instance Structural Text where
  type StructKind Text = 'StructString
  toStructValue = StringValue
  fromStructValue (StringValue t) = t

instance Structural Scientific where
  type StructKind Scientific = 'StructNumber
  toStructValue = NumberValue
  fromStructValue (NumberValue s) = s

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

instance
  ( Structural a
  , Structural b
  ) => Structural (a, b) where
  type StructKind (a, b) = 'StructProduct
    '[ '("0", StructKind a), '("1", StructKind b) ]
  toStructValue (a, b) = ProductValue
    $ ProductCons Proxy (toStructValue a)
    $ ProductCons Proxy (toStructValue b)
    ProductNil
  fromStructValue (ProductValue (ProductCons _ a (ProductCons _ b ProductNil)))
    = (fromStructValue a, fromStructValue b)

instance
  ( Structural a
  , Structural b
  ) => Structural (Either a b) where
  type StructKind (Either a b) = 'StructSum
    '[ '("left", StructKind a), '("right", StructKind b) ]
  toStructValue = \case
    Left a -> SumValue $ ThisValue Proxy $ toStructValue a
    Right b -> SumValue $ ThatValue $ ThisValue Proxy $ toStructValue b
  fromStructValue (SumValue s) = case s of
    ThisValue _ a             -> Left $ fromStructValue a
    ThatValue (ThisValue _ b) -> Right $ fromStructValue b
    ThatValue (ThatValue _)   -> error "Impossible happened"

class GStructural (f :: * -> *) where
  type GStructKind f :: Structure
  gToStructValue :: f p -> StructureValue (GStructKind f)
  gFromStructValue :: StructureValue (GStructKind f) -> f p

-- | Top level instance for non-newtypes only
instance
  ( GStructural sub
  ) => GStructural (D1 (MetaData n m p 'False) sub) where
  type GStructKind (D1 (MetaData n m p 'False) sub) = GStructKind sub
  gToStructValue (M1 fp) = gToStructValue fp
  gFromStructValue v = M1 $ gFromStructValue v

-- If constructor uses record syntax, then it is encoded as a tagged
-- product (json object)
instance (GProduct sels) => GStructural (C1 ('MetaCons cn f 'True) sels) where
  type GStructKind (C1 ('MetaCons cn f 'True) sels) =
    StructProduct (GProdKind sels)
  gToStructValue (M1 fp) = ProductValue (gToProductValue fp)
  gFromStructValue (ProductValue pl) = M1 $ gFromProductValue pl

instance
  ( GSum (C1 ('MetaCons cn f 'False) sels)
  ) => GStructural (C1 ('MetaCons cn f 'False) sels) where
  type GStructKind (C1 ('MetaCons cn f 'False) sels) =
    StructSum (GSumKind (C1 ('MetaCons cn f 'False) sels))
  gToStructValue m1 = SumValue (gToSumValue m1)
  gFromStructValue (SumValue pl) = gFromSumValue pl

instance (GSum (l :+: r)) => GStructural (l :+: r) where
  type GStructKind (l :+: r) =
    StructSum (GSumKind (l :+: r))
  gToStructValue m1 = SumValue (gToSumValue m1)
  gFromStructValue (SumValue pl) = gFromSumValue pl

class GProduct (f :: * -> *) where
  type GProdKind f :: [(Symbol, Structure)]
  gToProductValue :: f p -> ProductValueL (GProdKind f)
  gFromProductValue :: ProductValueL (GProdKind f) -> f p

instance
  ( Structural typ
  , KnownSymbol n
  ) => GProduct (S1 ('MetaSel ('Just n) a b c) (Rec0 typ)) where
  type GProdKind (S1 ('MetaSel ('Just n) a b c) (Rec0 typ)) =
    '[ '(n, StructKind typ) ]
  gToProductValue (M1 (K1 typ)) =
    ProductCons (Proxy @n) (toStructValue typ) ProductNil

instance
  ( GProduct l
  , GProduct r
  ) => GProduct (l :*: r) where
  type GProdKind (l :*: r)

class GSum (f :: * -> *) where
  type GSumKind f :: [(Symbol, Structure)]
  gToSumValue :: f p -> SumValueL (GSumKind f)
  gFromSumValue :: SumValueL (GSumKind f) -> f p
