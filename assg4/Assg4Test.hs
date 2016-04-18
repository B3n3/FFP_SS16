module Assg4Test where

import Test.HUnit
import Data.Monoid
import Control.Monad
import Assg4


main :: IO Counts
main = runTestTT $ TestList [test1BinomDyn, test2BinomDyn,
        test1Stack]

-- Ex1 Tests

succesor :: (Ord node, Num node) => node -> [node]
succesor x
    | x < 5    = [x + 1, x + 2]
    | otherwise = []

goal x
    | x == 3   = True
    | x == 2   = True
    | otherwise = False

test1Stack = TestCase $ assertEqual "BFS"
                [2, 2, 3]
                $ searchBfs succesor goal 0


-- Binom Tests

test1BinomDyn = TestCase $ assertEqual "BinomDyn"
                19448
                $ binomDyn (17,7)
test2BinomDyn = TestCase $ assertEqual "BinomDyn"
                67910864
                $ binomDyn (98,5)

