module Tolstoy.Structure.Kind where

import           GHC.TypeLits
import qualified Prelude as P

-- | Kind for describing the structure of the document
data Structure
  = String
  | Number
  | Bool
  | Null
  | Optional Structure
  | Vector Structure
  | Sum [(Symbol, Structure)]
  | Product [(Symbol, Structure)]
