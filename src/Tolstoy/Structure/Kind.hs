module Tolstoy.Structure.Kind where

import GHC.TypeLits

-- | Kind for describing the structure of the document
data Structure
  = StructString
  | StructNumber
  | StructBool
  | StructNull
  | StructOptional Structure
  | StructVector Structure
  | StructSum [(Symbol, Structure)]
  | StructProduct [(Symbol, Structure)]
