module Tolstoy.Structure.Aux where

import Tolstoy.Structure.Kind

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

-- | Where the second sum is placed in first
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
