module Assg2Test where

import Test.HUnit
import Data.Monoid
import Control.Monad
import Assg2

test1 = TestCase $ assertEqual "Decimals wrong"
            [1, 3, 9, 2]
            (decimals 1392)

test2 = TestCase $ assertEqual "Decimals wrong"
            []
            (decimals 0)

test3 = TestCase $ assertEqual "Primes wrong"
            [2,3,5,7,11,13,17,19,23,29]
            (take 10 primes)

test4 = TestCase $ assertEqual "Goedle number (gz) wrong"
            144
            (gz 42)

test5 = TestCase $ assertEqual "Goedle number (gz) wrong"
            402
            (gz 400)

test6 = TestCase $ assertEqual "Goedle number (gzMT) wrong"
            144
            (gzMT 42)

test7 = TestCase $ assertEqual "Goedle number (gzMT) wrong"
            402
            (gzMT 400)

test8 = TestCase $ assertEqual "Goedle number stream (gzs) wrong"
            [2,4,8,16,32,64,128,256,512,2,6,18,54,162,486,1458,4374,13122,39366,4]
            (take 20 gzs)

test9 = TestCase $ assertEqual "Goedle number stream (gzsMT) wrong"
            [2,4,8,16,32,64,128,256,512,2,6,18,54,162,486,1458,4374,13122,39366,4]
            (take 20 gzsMT)

test10 = TestCase $ assertEqual "nOfOnesInBinary"
            2
            (nOfOnesInBinary 3)

test11 = TestCase $ assertEqual "isPrime - it is prime"
            True
            (isPrime 541)

test12 = TestCase $ assertEqual "isPrime - it is not prime"
            False
            (isPrime 542)

test13 = TestCase $ assertEqual "ddps"
            [3,5,6,7,9,10,11,12,13,14]
            (take 10 ddps)

test14 = TestCase $ assertEqual "ddps"
            31
            (ddps!!20)

test15 = TestCase $ assertEqual "ddpsMT"
            31
            (ddpsMT!!20)

test16 = TestCase $ assertEqual "ddpsMT"
            31
            (ddpsMT!!20)

test17 = TestCase $ assertEqual "3 ^ 10 should be 59049"
            59049
            (powMT 10)


main :: IO Counts
main = runTestTT $ TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9,
            test10, test11, test12, test13, test14, test15, test16, test17]
