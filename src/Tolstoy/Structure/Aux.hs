{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Tolstoy.Structure.Aux where

import GHC.TypeLits
import Tolstoy.Structure.Kind
import TypeFun.Data.Maybe

type ProdElem p tag = FromJust (ProdElemMay p tag)

-- | Finds product element by tag.
type family ProdElemMay (p :: ProductTree) (tag :: Symbol) :: Maybe Structure where
  ProdElemMay (Product1 tag s) tag = Just s
  ProdElemMay (Product2 l r) tag =
    OneOf (ProdElemMay l tag) (ProdElemMay r tag)
  ProdElemMay p t = Nothing

type SumElem s tag = FromJust (SumElemMay s tag)

-- | Finds sum element by tag.
type family SumElemMay (p :: SumTree) (tag :: Symbol) :: Maybe Structure where
  SumElemMay (Sum1 tag s) tag = Just s
  SumElemMay (Sum2 l r) tag = OneOf (SumElemMay l tag) (SumElemMay r tag)
  SumElemMay a b = Nothing

type family OneOf (a :: Maybe Structure) (b :: Maybe Structure) :: Maybe Structure where
  OneOf Nothing (Just b) = Just b
  OneOf (Just a) Nothing = Just a
  OneOf a b = Nothing


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
