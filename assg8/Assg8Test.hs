module Assg8Test where

import Test.HUnit
import Assg8

main :: IO Counts
main = runTestTT $ TestList [testSimpleMSS1, testSimpleMSS2]

testSimpleMSS1 = TestCase $ assertEqual "simpleMSS"
                [2,1]
                $ simpleMSS [-4,-3,-7,2,1,-2,-1,-4]

testSimpleMSS2 = TestCase $ assertEqual "simpleMSS"
                [7,1,-2,-1,-4,10]
                $ simpleMSS [-4,-3,-7,7,1,-2,-1,-4,10]