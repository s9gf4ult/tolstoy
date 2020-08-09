{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Tolstoy.Migration.Obvious where

import Data.Proxy
import GHC.Exts
import GHC.TypeLits
import Tolstoy.Structure

obviousMigration
  :: (Structural a, Structural b, Obvious (StructKind a) (StructKind b))
  => a
  -> b
obviousMigration = fromStructValue . obvious . toStructValue

-- | Class of obvious transformations of the structure values between each other
class Obvious (s1 :: Structure) (s2 :: Structure) where
  obvious :: StructureValue s1 -> StructureValue s2


-- $ One to one transform


instance Obvious 'StructString 'StructString where
  obvious = id

instance Obvious 'StructNumber 'StructNumber where
  obvious = id

instance Obvious 'StructBool 'StructBool where
  obvious = id

instance
  ( Obvious s1 s2
  ) => Obvious ('StructOptional s1) ('StructOptional s2) where
  obvious (OptionalValue ma) = OptionalValue $ obvious <$> ma

instance (Obvious s1 s2) => Obvious ('StructVector s1) ('StructVector s2) where
  obvious (VectorValue va) = VectorValue $ obvious <$> va

instance
  ( ObviousProduct s1 s2 (WhereProd s1 s2)
  ) => Obvious ('StructProduct s1) ('StructProduct s2) where
  obvious (ProductValue s1) = ProductValue $ obviousProduct s1

instance
  ( SumLiftable s1 s2
  ) => Obvious ('StructSum s1) ('StructSum s2) where
  obvious (SumValue s1) = SumValue $ obviousSum s1

-- $ Lift to optional

instance (Obvious 'StructString s) => Obvious 'StructString ('StructOptional s) where
  obvious = OptionalValue . Just . obvious

instance (Obvious 'StructNumber s) => Obvious 'StructNumber ('StructOptional s) where
  obvious = OptionalValue . Just . obvious

instance (Obvious 'StructBool s) => Obvious 'StructBool ('StructOptional s) where
  obvious = OptionalValue . Just . obvious

instance
  ( Obvious ('StructVector s1) s2
  ) => Obvious ('StructVector s1) ('StructOptional s2) where
  obvious = OptionalValue . Just . obvious

instance
  ( Obvious ('StructSum s1) s2
  ) => Obvious ('StructSum s1) ('StructOptional s2) where
  obvious = OptionalValue . Just . obvious

instance
  ( Obvious ('StructProduct s1) s2
  ) => Obvious ('StructProduct s1) ('StructOptional s2) where
  obvious = OptionalValue . Just . obvious

-- $ Lift to vector

instance (Obvious 'StructString s) => Obvious 'StructString ('StructVector s) where
  obvious = VectorValue . pure . obvious

instance (Obvious 'StructNumber s) => Obvious 'StructNumber ('StructVector s) where
  obvious = VectorValue . pure . obvious

instance (Obvious 'StructBool s) => Obvious 'StructBool ('StructVector s) where
  obvious = VectorValue . pure . obvious

instance
  ( Obvious ('StructOptional s1) s2
  ) => Obvious ('StructOptional s1) ('StructVector s2) where
  obvious = VectorValue . pure . obvious

instance
  ( Obvious ('StructSum s1) s2
  ) => Obvious ('StructSum s1) ('StructVector s2) where
  obvious = VectorValue . pure . obvious

instance
  ( Obvious ('StructProduct s1) s2
  ) => Obvious ('StructProduct s1) ('StructVector s2) where
  obvious = VectorValue . pure . obvious


-- -- $ Change the container

-- instance (Obvious s1 s2) => Obvious ('StructOptional s1) ('StructVector s2) where
--   obvious (OptionalValue v) = case v of
--     Just s1 -> VectorValue $ pure $ obvious s1
--     Nothing -> VectorValue mempty

-- $ Wrap into the product

instance
  ( Obvious 'StructString s
  , KnownSymbol n
  ) => Obvious 'StructString ('StructProduct ('Product1 n s)) where
  obvious s1 = ProductValue $ Product1Value Proxy (obvious s1)

instance
  ( Obvious 'StructNumber s
  , KnownSymbol n
  ) => Obvious 'StructNumber ('StructProduct ('Product1 n s)) where
  obvious s1 = ProductValue $ Product1Value Proxy (obvious s1)

instance
  ( Obvious 'StructBool s
  , KnownSymbol n
  ) => Obvious 'StructBool ('StructProduct ('Product1 n s)) where
  obvious s1 = ProductValue $ Product1Value Proxy (obvious s1)

instance
  ( Obvious ('StructOptional s1) s2
  , KnownSymbol n
  ) => Obvious ('StructOptional s1) ('StructProduct ('Product1 n s2)) where
  obvious s1 = ProductValue $ Product1Value Proxy (obvious s1)

instance
  ( Obvious ('StructVector s1) s2
  , KnownSymbol n
  ) => Obvious ('StructVector s1) ('StructProduct ('Product1 n s2)) where
  obvious s1 = ProductValue $ Product1Value Proxy (obvious s1)

-- $ Unwrap from single element product

-- instance
--   ( Obvious s 'StructString
--   ) => Obvious ('StructSum ('Sum1 n s)) 'StructString where
--   obvious (SumValue (Sum1Value _proxy s1)) = obvious s1

-- instance
--   ( Obvious s 'StructNumber
--   ) => Obvious ('StructSum ('Sum1 n s)) 'StructNumber where
--   obvious (SumValue (Sum1Value _proxy s1)) = obvious s1

-- instance
--   ( Obvious s 'StructBool
--   ) => Obvious ('StructSum ('Sum1 n s)) 'StructBool where
--   obvious (SumValue (Sum1Value _proxy s1)) = obvious s1

-- instance
--   ( Obvious s1 ('StructOptional s2)
--   ) => Obvious ('StructSum ('Sum1 n s1)) ('StructOptional s2) where
--   obvious (SumValue (Sum1Value _proxy s1)) = obvious s1

-- instance
--   ( Obvious s1 ('StructVector s2)
--   ) => Obvious ('StructSum ('Sum1 n s1)) ('StructVector s2) where
--   obvious (SumValue (Sum1Value _proxy s1)) = obvious s1

-- $ Wrap in single element sum

instance
  ( Obvious 'StructString s
  , KnownSymbol n
  ) => Obvious 'StructString ('StructSum ('Sum1 n s)) where
  obvious s1 = SumValue $ Sum1Value Proxy (obvious s1)

instance
  ( Obvious 'StructNumber s
  , KnownSymbol n
  ) => Obvious 'StructNumber ('StructSum ('Sum1 n s)) where
  obvious s1 = SumValue $ Sum1Value Proxy (obvious s1)

instance
  ( Obvious 'StructBool s
  , KnownSymbol n
  ) => Obvious 'StructBool ('StructSum ('Sum1 n s)) where
  obvious s1 = SumValue $ Sum1Value Proxy (obvious s1)

instance
  ( Obvious ('StructOptional s1) s2
  , KnownSymbol n
  ) => Obvious ('StructOptional s1) ('StructSum ('Sum1 n s2)) where
  obvious s1 = SumValue $ Sum1Value Proxy (obvious s1)

instance
  ( Obvious ('StructVector s1) s2
  , KnownSymbol n
  ) => Obvious ('StructVector s1) ('StructSum ('Sum1 n s2)) where
  obvious s1 = SumValue $ Sum1Value Proxy (obvious s1)

-- $ Unwrap single element sum

-- instance
--   ( Obvious s 'StructString
--   ) => Obvious ('StructProduct ('Product1 n s)) 'StructString where
--   obvious (ProductValue (Product1Value _proxy s1)) = obvious s1

-- instance
--   ( Obvious s 'StructNumber
--   ) => Obvious ('StructProduct ('Product1 n s)) 'StructNumber where
--   obvious (ProductValue (Product1Value _proxy s1)) = obvious s1

-- instance
--   ( Obvious s 'StructBool
--   ) => Obvious ('StructProduct ('Product1 n s)) 'StructBool where
--   obvious (ProductValue (Product1Value _proxy s1)) = obvious s1

-- instance
--   ( Obvious s1 ('StructOptional s2)
--   ) => Obvious ('StructProduct ('Product1 n s1)) ('StructOptional s2) where
--   obvious (ProductValue (Product1Value _proxy s1)) = obvious s1

-- instance
--   ( Obvious s1 ('StructVector s2)
--   ) => Obvious ('StructProduct ('Product1 n s1)) ('StructVector s2) where
--   obvious (ProductValue (Product1Value _proxy s1)) = obvious s1

-- $ Product transformation

class
  ( goto ~ WhereProd p1 p2
  ) => ObviousProduct p1 p2 (goto :: Maybe Goto) where
  obviousProduct :: ProductTreeValue p1 -> ProductTreeValue p2

instance ObviousProduct 'Product0 'Product0 ('Just 'C) where
  obviousProduct = id

instance
  ( Obvious s1 s2
  ) => ObviousProduct ('Product1 n s1) ('Product1 n s2) ('Just 'C) where
  obviousProduct (Product1Value n s1) = Product1Value n $ obvious s1

instance
  ( ObviousProduct l ('Product1 n s) (WhereProd l ('Product1 n s))
  , ('Just 'L) ~ (WhereProd ('Product2 l r) ('Product1 n s))
  ) => ObviousProduct ('Product2 l r) ('Product1 n s) ('Just 'L) where
  obviousProduct (Product2Value l _) = obviousProduct l

instance
  ( ObviousProduct r ('Product1 n s) (WhereProd r ('Product1 n s))
  , ('Just 'R) ~ (WhereProd ('Product2 l r) ('Product1 n s))
  ) => ObviousProduct ('Product2 l r) ('Product1 n s) ('Just 'R) where
  obviousProduct (Product2Value _ r) = obviousProduct r

-- | Adding some optional value is trivial
instance
  ( 'Nothing ~ (WhereProd p ('Product1 n ('StructOptional s)))
  , KnownSymbol n
  ) => ObviousProduct p ('Product1 n ('StructOptional s)) 'Nothing where
  obviousProduct _ = Product1Value Proxy (OptionalValue Nothing)

-- | Adding some vector value is trivial
instance
  ( 'Nothing ~ (WhereProd p ('Product1 n ('StructVector s)))
  , KnownSymbol n
  ) => ObviousProduct p ('Product1 n ('StructVector s)) 'Nothing where
  obviousProduct _ = Product1Value Proxy (VectorValue mempty)

instance
  ( ObviousProduct p l (WhereProd p l)
  , ObviousProduct p r (WhereProd p r)
  , ('Just 'C) ~ (WhereProd p ('Product2 l r))
  ) => ObviousProduct p ('Product2 l r) ('Just 'C) where
  obviousProduct p = Product2Value (obviousProduct p) (obviousProduct p)

-- $ Lifting any element of sum

class
  ( goto ~ WhereSum tree ('Sum1 n s)
  ) => LiftSum n s tree (goto :: Maybe Goto) where
  liftSum :: SumTreeValue ('Sum1 n s) -> SumTreeValue tree

instance (Obvious s1 s2) => LiftSum n s1 ('Sum1 n s2) ('Just 'C) where
  liftSum (Sum1Value p s1) = Sum1Value p $ obvious s1

instance
  ( LiftSum n s l (WhereSum l ('Sum1 n s))
  , ('Just 'L) ~ WhereSum ('Sum2 l r) ('Sum1 n s)
  ) => LiftSum n s ('Sum2 l r) ('Just 'L) where
  liftSum s = Sum2Left $ liftSum s

instance
  ( LiftSum n s r (WhereSum r ('Sum1 n s))
  , ('Just 'R) ~ WhereSum ('Sum2 l r) ('Sum1 n s)
  ) => LiftSum n s ('Sum2 l r) ('Just 'R) where
  liftSum s = Sum2Right $ liftSum s

obviousSum :: (SumLiftable s1 s2) => SumTreeValue s1 -> SumTreeValue s2
obviousSum a = case a of
  Sum1Value _p _s -> liftSum a
  Sum2Left l      -> obviousSum l
  Sum2Right r     -> obviousSum r


-- $ Helper types

type family SumLiftable (s1 :: SumTree) (s2 :: SumTree) :: Constraint where
  SumLiftable (Sum1 n s1) s2 = LiftSum n s1 s2 (WhereSum s2 ('Sum1 n s1))
  SumLiftable (Sum2 l r) s = (SumLiftable l s, SumLiftable r s)

-- | Where to direct the instance. To the left branch
-- construction/deconstruction or to the right
data Goto = L | R | C

-- | Where is the second product is placed in first
type family WhereProd (p1 :: ProductTree) (p2 :: ProductTree) :: Maybe Goto where
  WhereProd Product0 Product0                 = Just C
  WhereProd (Product1 n s1) (Product1 n s2)   = Just C
  WhereProd (Product2 l r) (Product1 n s)     =
    EitherLR (WhereProd l (Product1 n s)) (WhereProd r (Product1 n s))
  WhereProd (Product2 l1 r1) (Product2 l2 r2) = Just C
  WhereProd p1 p2                             = Nothing

-- | Where is the second sum is placed in first
type family WhereSum (s1 :: SumTree) (s2 :: SumTree) :: Maybe Goto where
  WhereSum (Sum1 n s1) (Sum1 n s2)   = Just C
  WhereSum (Sum2 l r) (Sum1 n s)     = EitherLR
    (WhereSum l (Sum1 n s)) (WhereSum r (Sum1 n s))
  WhereSum (Sum2 l1 r1) (Sum2 l2 r2) = Just C
  WhereSum a b                       = Nothing

type family EitherLR (l :: Maybe Goto) (r :: Maybe Goto) :: Maybe Goto where
  EitherLR Nothing (Just r) = Just R
  EitherLR (Just l) Nothing = Just L
  EitherLR l r              = Nothing
