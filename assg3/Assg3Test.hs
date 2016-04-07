module Assg3Test where

import Test.HUnit
import Data.Monoid
import Control.Monad
import Assg3

test1 = TestCase $ assertEqual "Dummy"
            0
            0

main :: IO Counts
main = runTestTT $ TestList [test1, testTransform]


-- Ex 1 tests

testTransform = TestCase $ assertEqual "Test transform"
					((1, 0), 3, (7, 7))
					$ transform ((B, Eins), 3, (H, Acht))