module Assg6Test where

import Test.HUnit
import Assg6


main :: IO Counts
main = runTestTT $ TestList [
    testTokenize,
    testToString1, testToString2,
    testInterpret1, testInterpret2, testInterpret3, testInterpret4]

testTokenize = TestCase $ assertEqual "tokenize"
                [Value 123, Add, Value (-123), Wrong, Wrong, Mult, Value (-1)]
                $ tokenize "123+-123 wr * -01"


testToString1 = TestCase $ assertEqual "toString"
                "Error"
                $ toString []

testToString2 = TestCase $ assertEqual "toString"
                "35 -2 1"
                $ toString [1, -2, 35]

testInterpret1 = TestCase $ assertEqual "interpret"
                []
                $ interpret []

testInterpret2 = TestCase $ assertEqual "interpret"
                []
                $ interpret [Value 123, Value 1, Wrong, Mult]

testInterpret3 = TestCase $ assertEqual "interpret"
                []
                $ interpret [Value 123, Mult]

testInterpret4 = TestCase $ assertEqual "interpret"
                [3, 245]
                $ interpret [Value 123, Value 2, Mult, Value (-1), Add, Value 3]