module Assg8Test where

import Test.HUnit
import Assg8

main :: IO Counts
main = runTestTT $ TestList [testSimpleMSS]

testSimpleMSS = TestCase $ assertEqual "simpleMSS"
                [2,1]
                $ simpleMSS [-4,-3,-7,2,1,-2,-1,-4]