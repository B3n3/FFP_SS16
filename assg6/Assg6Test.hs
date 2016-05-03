module Assg6Test where

import Test.HUnit
import Assg6


main :: IO Counts
main = runTestTT $ TestList [
    testTokenize,
    testToString1, testToString2]

testTokenize = TestCase $ assertEqual "tokenize"
                [Value 123, Add, Value (-123), Wrong, Wrong, Mult, Value (-1)]
                $ tokenize "123+-123 wr * -01"


testToString1 = TestCase $ assertEqual "toString"
                "Error"
                $ toString []

testToString2 = TestCase $ assertEqual "toString"
                "35 -2 1"
                $ toString [1, -2, 35]