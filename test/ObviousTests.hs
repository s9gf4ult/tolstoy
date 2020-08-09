module ObviousTests where

import ObviousTests.Alter
import ObviousTests.Alter2
import ObviousTests.Main
import Test.Tasty as Test
import Test.Tasty.QuickCheck
import Tolstoy.Migration.Obvious

data1To2 :: Prod1 -> Prod2
data1To2 = obviousMigration

data1To3 :: Prod1 -> Prod3
data1To3 = obviousMigration

data1To4 :: Prod1 -> Prod4
data1To4 = obviousMigration

data1To5 :: Prod1 -> Prod5
data1To5 = obviousMigration

sumAtoB :: SumA -> SumB
sumAtoB = obviousMigration

sumAtoC :: SumA -> SumC
sumAtoC = obviousMigration

test_Obvious :: TestTree
test_Obvious = testGroup "Obvious migrations"
  [ testProperty "Prod1 -> Prod2" $ total . data1To2
  , testProperty "Prod1 -> Prod3" $ total . data1To3
  , testProperty "Prod1 -> Prod4" $ total . data1To4
  , testProperty "Prod1 -> Prod5" $ total . data1To5
  , testProperty "SumA -> SumB" $ total . sumAtoB
  , testProperty "SumA -> SumC" $ total . sumAtoC
  ]
