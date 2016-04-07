module Assg3Test where

import Test.HUnit
import Data.Monoid
import Control.Monad
import Assg3

test1 = TestCase $ assertEqual "Dummy"
            0
            0

main :: IO Counts
main = runTestTT $ TestList [test1]
