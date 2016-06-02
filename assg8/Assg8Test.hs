module Assg8Test where

import Test.HUnit
import Data.Array
import Assg8

main :: IO Counts
main = runTestTT $ TestList [testSimpleMSS1, testSimpleMSS2, testSimpleMSS3,
                             testSmartMSS1, testSmartMSS2, testSmartMSS3,
                             testOutCamp,
                             testMakeCamp, testExpandSolutions, testSimpleCamp]

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

testMakeCamp = TestCase $ assertEqual "makeCamp"
                (listArray ((1,1),(8,8))
                   [Tree,  Empty, Empty, Empty, Empty, Empty, Empty, Empty,
                    Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
                    Empty, Empty, Empty, Empty, Tree,  Empty, Empty, Empty,
                    Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
                    Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
                    Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
                    Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
                    Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty])
                $ makeCamp [(1, 1), (3, 5)]

testExpandSolutions = TestCase $ assertEqual "expandSolutions"
                [
                   (listArray ((1,1),(8,8))
                       [Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                        Tree, Empty,Tree, Tree, Tree, Tree, Tree, Tree,
                        Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                        Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                        Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                        Tree, Empty,Tree, Tree, Tree, Tree, Tree, Tree,
                        Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                        Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree]),
                   (listArray ((1,1),(8,8))
                       [Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                        Tree, Empty,Tree, Tree, Tree, Tree, Tree, Tree,
                        Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                        Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                        Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                        Tree, Tent, Tree, Tree, Tree, Tree, Tree, Tree,
                        Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                        Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree]),
                   (listArray ((1,1),(8,8))
                        [Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                         Tree, Tent,Tree, Tree, Tree, Tree, Tree, Tree,
                         Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                         Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                         Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                         Tree, Empty, Tree, Tree, Tree, Tree, Tree, Tree,
                         Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                         Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree]),
                   (listArray ((1,1),(8,8))
                        [Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                         Tree, Tent, Tree, Tree, Tree, Tree, Tree, Tree,
                         Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                         Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                         Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                         Tree, Tent, Tree, Tree, Tree, Tree, Tree, Tree,
                         Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                         Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree])
                ]
                $ expandSolutions
                    (listArray ((1,1),(8,8))
                       [Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                        Tree, Empty,Tree, Tree, Tree, Tree, Tree, Tree,
                        Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                        Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                        Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                        Tree, Empty,Tree, Tree, Tree, Tree, Tree, Tree,
                        Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree,
                        Tree, Tree, Tree, Tree, Tree, Tree, Tree, Tree])

testSimpleCamp = TestCase $ assertEqual "simpleCamp, should find no solution"
                (listArray ((1,1),(8,8))
                   [Tree,Tree,Empty,Tree,Tree,Tree,Empty,Tree,Tree,Empty,Tree,Tree,Tree,Empty,Tree,Tree,Empty,Tree,Tree,Tree,Empty,Tree,Tree,Tree,Tree,Tree,Tree,Empty,Tree,Tree,Tree,Empty,Tree,Tree,Empty,Tree,Tree,Tree,Empty,Tree,Tree,Empty,Tree,Tree,Tree,Empty,Tree,Tree,Empty,Tree,Tree,Tree,Empty,Tree,Tree,Tree,Tree,Tree,Tree,Empty,Tree,Tree,Tree,Empty])
                $ simpleCamp
                    [(i, j) | i <- [1..8], j <- [1..8], (i + j) `mod` 4 /= 0]
                    [3, 1, 3, 1, 1, 3, 0, 4]
                    [4, 0, 3, 1, 3, 1, 3, 1]