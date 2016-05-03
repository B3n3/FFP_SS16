module Assg6Test where

import Test.HUnit
import Assg6


main :: IO Counts
main = runTestTT $ TestList [testTokenize]

testTokenize = TestCase $ assertEqual "tokenize"
                [Value 123, Add, Value (-123), Wrong, Wrong, Mult, Value (-1)]
                $ tokenize "123+-123 wr * -01"