module Tolstoy.Structure.Class where

import Tolstoy.Structure.Kind
import Tolstoy.Structure.Value

class Structural s where
  type StructKind s :: Structure
  toStructValue :: s -> StructureValue (StructKind s)
  fromStructValue :: StructureValue (StructKind s) -> s
