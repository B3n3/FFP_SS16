module Assg4Test where

import Test.HUnit
import Data.Monoid
import Control.Monad
import Assg4


main :: IO Counts
main = runTestTT $ TestList [test1BinomDyn, test2BinomDyn,
        test1Bfs,
        testFromVirtual, testToVirtual,
        genSuccTest, spgSuccTest,
        spgBfsTest,
        spgDfsTest]

-- Ex1 Tests

succesor :: (Ord node, Num node) => node -> [node]
succesor x
    | x < 5    = [x + 1, x + 2]
    | otherwise = []

goal x
    | x == 3   = True
    | x == 2   = True
    | otherwise = False

test1Bfs = TestCase $ assertEqual "BFS"
                [2, 2, 3]
                $ searchBfs succesor goal 0

-- Ex 2 Tests

testFromVirtual = TestCase $ assertEqual "from virtual"
                (A, Zwei)
                $ fromVirtual (0, 1)

testToVirtual = TestCase $ assertEqual "to virtual"
                (7, 2)
                $ toVirtual (H, Drei)

genSuccTest = TestCase $ assertEqual "gen succ positions"
                [(1, 2), (2, 1)]
                $ genSucc (0, 0)

spgSuccTest = TestCase $ assertEqual "spg succesor test"
                [
                    ((A, Eins), (H, Acht), 12, (B, Drei), 1, [((A, Eins), (B, Drei))]),
                    ((A, Eins), (H, Acht), 12, (C, Zwei), 1, [((A, Eins), (C, Zwei))])
                ]
                $ spgSucc ((A, Eins), (H, Acht), 12, (A, Eins), 0, [])

spgBfsTest = TestCase $ assertEqual "spg BFS test"
                [
                    [((A,Eins),(C,Zwei))],
                    [((A,Eins),(B,Drei)),((B,Drei),(D,Vier)),((D,Vier),(C,Zwei))],
                    [((A,Eins),(B,Drei)),((B,Drei),(A,Eins)),((A,Eins),(C,Zwei))]
                ]
                $ spgBfs (A, Eins) (C, Zwei) 3

spgDfsTest = TestCase $ assertEqual "spg DFS test"
                [
                    [((A,Eins),(B,Drei)),((B,Drei),(D,Vier)),((D,Vier),(C,Zwei))],
                    [((A,Eins),(B,Drei)),((B,Drei),(A,Eins)),((A,Eins),(C,Zwei))],
                    [((A,Eins),(C,Zwei))]
                ]
                $ spgDfs (A, Eins) (C, Zwei) 3


-- Binom Tests

test1BinomDyn = TestCase $ assertEqual "BinomDyn"
                19448
                $ binomDyn (17,7)
test2BinomDyn = TestCase $ assertEqual "BinomDyn"
                67910864
                $ binomDyn (98,5)

