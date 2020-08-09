module ObviousTests where

import ObviousTests.Alter
import ObviousTests.Alter2
import ObviousTests.Main
import Test.Tasty as Test
import Test.Tasty.QuickCheck
import Tolstoy.Migration.Obvious
import Tolstoy.Structure

data1To2 :: Prod1 -> Prod2
data1To2 d1 = fromStructValue $ obvious $ toStructValue d1

data1To3 :: Prod1 -> Prod3
data1To3 d1 = fromStructValue $ obvious $ toStructValue d1

data1To4 :: Prod1 -> Prod4
data1To4 d1 = fromStructValue $ obvious $ toStructValue d1

sumAtoB :: SumA -> SumB
sumAtoB s1 = fromStructValue $ obvious $ toStructValue s1

sumAtoC :: SumA -> SumC
sumAtoC s1 = fromStructValue $ obvious $ toStructValue s1

test_Obvious :: TestTree
test_Obvious = testGroup "Obvious migrations"
  [ testProperty "Prod1 -> Prod2" $ total . data1To2
  , testProperty "Prod1 -> Prod3" $ total . data1To3
  , testProperty "Prod1 -> Prod4" $ total . data1To4
  , testProperty "SumA -> SumB" $ total . sumAtoB
  , testProperty "SumA -> SumC" $ total . sumAtoC
  ]
