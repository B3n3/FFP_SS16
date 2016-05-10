module Assg7Test where

import Test.HUnit
import Assg7

main :: IO Counts
main = runTestTT $ TestList [test1OccS, test2OccS, test3OccS, test4OccS]

test1OccS = TestCase $ assertEqual "one occurrence"
                [(1, 3)]
                $ occS "kinder" "ind"

test2OccS = TestCase $ assertEqual "2 occurrences"
                [(1, 3), (6, 8)]
                $ occS "kinderind" "ind"

test3OccS = TestCase $ assertEqual "no occurrence"
                []
                $ occS "jkljhkhjh" "ind"

test4OccS = TestCase $ assertEqual "2 occurrences next to each other "
                [(0, 2), (3, 5)]
                $ occS "indind" "ind"