module Assg1Test where

import Test.HUnit
import Data.Monoid
import Control.Monad
import Assg1

test1 = TestCase $ assertEqual "Should return first elements of powers of three"
            [1, 3, 9, 27]
            (take 4 powersOfThrees)

test2 = TestCase $ assertEqual "Some text here"
            [[1],[1,1],[1,3,1],[1,7,6,1],[1,15,25,10,1]]
            (take 5 sd)

test3 = TestCase $ assertEqual "Some text here"
            [1,2,5,15,52]
            (take 5 bn)

main :: IO Counts
main = runTestTT $ TestList [test1, test2, test3]