module Assg5Test where

import Test.HUnit
import Data.Monoid
import Control.Monad
import Data.Array
import Assg5


main :: IO Counts
main = runTestTT $ TestList [masTest1, amasTest1, amasTest2,
    lmasTest1, lmasTest2, lmasTest3,
    minIndexTest1, minIndexTest2, minIndexTest3, minIndexTest4, 
    minIndexTest5, minIndexTest6, minIndexTest7, minIndexTest8]

a :: Array Int Int
a = array (1,9) [(1,3),(2,(-5)),(3,0),(4,9),(5,2),(6,(-1)),(7,2),(8,(-5)),(9,1)]

b :: Array Int Int
b = array (1,9) [(1,3),(2,(-1)),(3,(-2)),(4,9),(5,2),(6,(-1)),(7,2),(8,0),(9,(-1))]

c :: Array Int Int
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


minIndexTest1 = TestCase $ assertEqual "minIndex"
                    4
                    $ minIndex a (>5)

minIndexTest2 = TestCase $ assertEqual "minIndex"
                    2
                    $ minIndex a (<0)

minIndexTest3 = TestCase $ assertEqual "minIndex"
                    3
                    $ minIndex a (even)

minIndexTest4 = TestCase $ assertEqual "minIndex"
                    1
                    $ minIndex b (odd)

data Week = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq,Ord,Ix,Show)

d :: Array Week String
d = array (Tue,Sat) [(Wed,"work"),(Thu,"study"),(Tue,"study"),(Fri,"chill"),(Sat,"relax")]

minIndexTest5 = TestCase $ assertEqual "minIndex"
                    Sat
                    $ minIndex d (=="relax")

minIndexTest6 = TestCase $ assertEqual "minIndex"
                    Wed
                    $ minIndex d (=="work")

minIndexTest7 = TestCase $ assertEqual "minIndex"
                    Fri
                    $ minIndex d (=="chill")

minIndexTest8 = TestCase $ assertEqual "minIndex"
                    Tue
                    $ minIndex d (/="chill")