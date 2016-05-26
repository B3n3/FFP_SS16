module Assg8Test where

import Test.HUnit
import Data.Array
import Assg8

main :: IO Counts
main = runTestTT $ TestList [testSimpleMSS1, testSimpleMSS2, testSimpleMSS3,
                             testSmartMSS1, testSmartMSS2, testSmartMSS3,
                             testOutCamp]

testSimpleMSS1 = TestCase $ assertEqual "simpleMSS"
                [2,1]
                $ simpleMSS [-4,-3,-7,2,1,-2,-1,-4]

testSimpleMSS2 = TestCase $ assertEqual "simpleMSS"
                [7,1,-2,-1,-4,10]
                $ simpleMSS [-4,-3,-7,7,1,-2,-1,-4,10]

testSimpleMSS3 = TestCase $ assertEqual "simpleMSS"
                [9,-1,4]
                $ simpleMSS [9,-1,4,-5,-7,7,1,-2,-1,-4,10]

testSmartMSS1 = TestCase $ assertEqual "smartMSS"
                [2,1]
                $ smartMSS [-4,-3,-7,2,1,-2,-1,-4]

testSmartMSS2 = TestCase $ assertEqual "smartMSS"
                [7,1,-2,-1,-4,10]
                $ smartMSS [-4,-3,-7,7,1,-2,-1,-4,10]

testSmartMSS3 = TestCase $ assertEqual "smartMSS"
                [9,-1,4]
                $ smartMSS [9,-1,4,-5,-7,7,1,-2,-1,-4,10]

testOutCamp = TestCase $ assertEqual "outCamp"
                ["BZBZuuZB", "uuuuuuuu", "BZBZuuZB", "uuuuuuuu", "BZBZuuZB", "uuuuuuuu", "uuuuuuuu", "BZBZuuZB"]
                $ outCamp $ listArray ((1,1),(8,8)) [Tree,  Tent,  Tree,  Tent,  Empty, Empty, Tent,  Tree,
                                                     Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
                                                     Tree,  Tent,  Tree,  Tent,  Empty, Empty, Tent,  Tree,
                                                     Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
                                                     Tree,  Tent,  Tree,  Tent,  Empty, Empty, Tent,  Tree,
                                                     Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
                                                     Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
                                                     Tree,  Tent,  Tree,  Tent,  Empty, Empty, Tent,  Tree]