module Assg5Test where

import Test.HUnit
import Data.Monoid
import Control.Monad
import Array
import Assg5


main :: IO Counts
main = runTestTT $ TestList [ ]

a = array (1,9) [(1,3),(2,(-5)),(3,0),(4,9),(5,2),(6,(-1)),(7,2),(8,(-5)),(9,1)]
b = array (1,9) [(1,3),(2,(-1)),(3,(-2)),(4,9),(5,2),(6,(-1)),(7,2),(8,0),(9,(-1))]

masTest1 = TestCase $ assertEqual "mas"
                12
                $ mas a

amasTest1 = TestCase $ assertEqual "amas"
                [(3,7),(4,7)]
                $ amas a

amasTest2 = TestCase $ assertEqual "amas"
                [(1,7),(1,8),(4,7),(4,8)]
                $ amas b

