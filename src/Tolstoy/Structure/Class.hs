module Tolstoy.Structure.Class where

import Data.Proxy
import Data.Scientific
import Data.Text (Text)
import Data.Vector (Vector)
import Tolstoy.Structure.Kind
import Tolstoy.Structure.Value

class Structural s where
  type StructKind s :: Structure
  toStructValue :: s -> StructureValue (StructKind s)
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

instance Structural () where
  type StructKind () = 'StructNull
  toStructValue () = NullValue
  fromStructValue NullValue = ()

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
