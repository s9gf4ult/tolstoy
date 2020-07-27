{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Tolstoy.Structure.Rep where

import           Data.Aeson
import           Data.Aeson.Types (Pair)
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.TypeLits
import           Tolstoy.Structure.Kind

-- data StructureRep :: Structure -> * where
--   StringRep   :: StructureRep StructString
--   NumberRep   :: StructureRep StructNumber
--   BoolRep     :: StructureRep StructBool
--   OptionalRep :: StructureRep s -> StructureRep (StructOptional s)
--   VectorRep   :: StructureRep s -> StructureRep (StructVector s)
--   SumRep      :: TaggedListRep l -> StructureRep (StructSum l)
--   ProductRep  :: TaggedListRep l -> StructureRep (StructProduct l)

-- data TaggedListRep :: [(Symbol, Structure)] -> * where
--   TaggedListNil :: TaggedListRep '[]
--   TaggedListCons
--     :: (KnownSymbol t)
--     => Proxy t
--     -> StructureRep s
--     -> TaggedListRep rest
--     -> TaggedListRep ('(t, s) ': rest)


-- instance ToJSON (StructureRep s) where
--   toJSON s = object $ mconcat
--     [ pure $ "type" .= stype
--     , ("argument" .=) <$> larg
--     , ("tags" .=) <$> tags
--     ]
--     where
--       stype :: Text
--       stype = case s of
--         StringRep      -> "string"
--         NumberRep      -> "number"
--         BoolRep        -> "bool"
--         OptionalRep {} -> "optional"
--         VectorRep {}   -> "vector"
--         SumRep    {}   -> "sum"
--         ProductRep {}  -> "product"
--       larg :: [Value]
--       larg = case s of
--         OptionalRep sub -> pure $ toJSON sub
--         VectorRep sub   -> pure $ toJSON sub
--         _               -> []
--       tags :: [Value]
--       tags = case s of
--         SumRep l     -> pure $ object $ taggedListJson l
--         ProductRep l -> pure $ object $ taggedListJson l
--         _            -> []

-- taggedListJson :: TaggedListRep l -> [Pair]
-- taggedListJson = \case
--   TaggedListNil -> []
--   TaggedListCons p rep rest ->
--     ((T.pack $ symbolVal p) .= rep)
--     : taggedListJson rest

-- -- | Materialize any structure type to it's representation
-- class KnownStructure (s :: Structure) where
--   structureRep :: StructureRep s

-- instance KnownStructure StructString where
--   structureRep = StringRep

-- instance KnownStructure StructNumber where
--   structureRep = NumberRep

-- instance KnownStructure StructBool where
--   structureRep = BoolRep

-- instance (KnownStructure s) => KnownStructure (StructOptional s) where
--   structureRep = OptionalRep structureRep

-- instance (KnownStructure s) => KnownStructure (StructVector s) where
--   structureRep = VectorRep structureRep

-- instance (KnownTaggedList l) => KnownStructure (StructSum l) where
--   structureRep = SumRep taggedListRep

-- instance (KnownTaggedList l) => KnownStructure (StructProduct l) where
--   structureRep = ProductRep taggedListRep

-- class KnownTaggedList (l :: [(Symbol, Structure)]) where
--   taggedListRep :: TaggedListRep l

-- instance KnownTaggedList '[] where
--   taggedListRep = TaggedListNil

-- instance (KnownTaggedList rest, KnownStructure s, KnownSymbol t)
--   => KnownTaggedList ('(t, s) ': rest) where
--   taggedListRep = TaggedListCons (Proxy @t) structureRep taggedListRep
