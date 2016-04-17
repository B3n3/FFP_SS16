module Assg4Test where

import Test.HUnit
import Data.Monoid
import Control.Monad
import Assg4


main :: IO Counts
main = runTestTT $ TestList [test1BinomDyn, test2BinomDyn]


-- Binom Tests

test1BinomDyn - TestCase $ assertEqual "BinomDyn"
                19448
                $ binomDyn (17,7)
test2BinomDyn - TestCase $ assertEqual "BinomDyn"
                67910864
                $ binomDyn (98,5)

