module Tolstoy.Structure.Kind where

import GHC.TypeLits

-- | Kind for describing the structure of the document
data Structure
  = StructString
  | StructNumber
  | StructBool
  | StructOptional Structure
  | StructVector Structure
  | StructSum SumTree
  | StructProduct ProductTree

data SumTree
  = Sum1 Symbol Structure
  | Sum2 SumTree SumTree

data ProductTree
  = Product0
  | Product1 Symbol Structure
  | Product2 ProductTree ProductTree

type StructureEmpty = StructProduct Product0
