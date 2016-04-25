module Assg5Test where

import Test.HUnit
import Data.Monoid
import Control.Monad
import Data.Array
import Assg5


main :: IO Counts
main = runTestTT $ TestList [masTest1, amasTest1, amasTest2,
    lmasTest1, lmasTest2, lmasTest3]

a = array (1,9) [(1,3),(2,(-5)),(3,0),(4,9),(5,2),(6,(-1)),(7,2),(8,(-5)),(9,1)]
b = array (1,9) [(1,3),(2,(-1)),(3,(-2)),(4,9),(5,2),(6,(-1)),(7,2),(8,0),(9,(-1))]
c = array (1,5) [(1,2),(2,3),(3,(-10)),(4,1),(5,4)]

masTest1 = TestCase $ assertEqual "mas"
                12
                $ mas a

amasTest1 = TestCase $ assertEqual "amas"
                [(3,7),(4,7)]
                $ amas a

amasTest2 = TestCase $ assertEqual "amas"
                [(1,7),(1,8),(4,7),(4,8)]
                $ amas b

lmasTest1 = TestCase $ assertEqual "lmas"
                (3, 7)
                $ lmas a

lmasTest2 = TestCase $ assertEqual "lmas"
                (1, 8)
                $ lmas b

lmasTest3 = TestCase $ assertEqual "lmas"
                (1, 2)
                $ lmas c