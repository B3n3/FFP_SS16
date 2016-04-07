module Assg3Test where

import Test.HUnit
import Data.Monoid
import Control.Monad
import Assg3


main :: IO Counts
main = runTestTT $ TestList [
    testTransform,
    testSpgGenerator1, testSpgGenerator2, testSpgGenerator3,
    testSpgFilter1, testSpgFilter2,
    testSpgSelektor1, testSpgSelektor2,
    testSpringer1, testSpringer2, testSpringer3, testSpringer4]


-- Ex 1 tests

testTransform = TestCase $ assertEqual "Test transform"
                    ((2, 1), 3, (8, 8))
                    $ transform ((B, Eins), 3, (H, Acht))


testSpgGenerator1 = TestCase $ assertEqual "Test spgGenerator with 0 moves"
                    ((0, 0), [[]])
                    $ spgGenerator ((1, 3), 0, (0, 0))

testSpgGenerator2 = TestCase $ assertEqual "Test spgGenerator with 1 moves"
                    ((4, 7), [[((1, 3), (-1, 4))], [((1, 3), (0, 5))], [((1, 3), (2, 5))], [((1, 3), (3, 4))],
                              [((1, 3), (3, 2))], [((1, 3), (2, 1))], [((1, 3), (0, 1))], [((1, 3), (-1, 2))]])
                    $ spgGenerator ((1, 3), 1, (4, 7))

testSpgGenerator3 = TestCase $ assertEqual "Test spgGenerator with 2 moves"
                    ((4,7),[[((1,3),(-1,4)),((-1,4),(-3,5))],[((1,3),(-1,4)),((-1,4),(-2, 6))],[((1,3),(-1,4)),((-1,4),( 0, 6))],[((1,3),(-1,4)),((-1,4),( 1,5))],
                            [((1,3),(-1,4)),((-1,4),( 1,3))],[((1,3),(-1,4)),((-1,4),( 0, 2))],[((1,3),(-1,4)),((-1,4),(-2, 2))],[((1,3),(-1,4)),((-1,4),(-3,3))],
                            [((1,3),( 0,5)),(( 0,5),(-2,6))],[((1,3),( 0,5)),(( 0,5),(-1, 7))],[((1,3),( 0,5)),(( 0,5),( 1, 7))],[((1,3),( 0,5)),(( 0,5),( 2,6))],
                            [((1,3),( 0,5)),(( 0,5),( 2,4))],[((1,3),( 0,5)),(( 0,5),( 1, 3))],[((1,3),( 0,5)),(( 0,5),(-1, 3))],[((1,3),( 0,5)),(( 0,5),(-2,4))],
                            [((1,3),( 2,5)),(( 2,5),( 0,6))],[((1,3),( 2,5)),(( 2,5),( 1, 7))],[((1,3),( 2,5)),(( 2,5),( 3, 7))],[((1,3),( 2,5)),(( 2,5),( 4,6))],
                            [((1,3),( 2,5)),(( 2,5),( 4,4))],[((1,3),( 2,5)),(( 2,5),( 3, 3))],[((1,3),( 2,5)),(( 2,5),( 1, 3))],[((1,3),( 2,5)),(( 2,5),( 0,4))],
                            [((1,3),( 3,4)),(( 3,4),( 1,5))],[((1,3),( 3,4)),(( 3,4),( 2, 6))],[((1,3),( 3,4)),(( 3,4),( 4, 6))],[((1,3),( 3,4)),(( 3,4),( 5,5))],
                            [((1,3),( 3,4)),(( 3,4),( 5,3))],[((1,3),( 3,4)),(( 3,4),( 4, 2))],[((1,3),( 3,4)),(( 3,4),( 2, 2))],[((1,3),( 3,4)),(( 3,4),( 1,3))],
                            [((1,3),( 3,2)),(( 3,2),( 1,3))],[((1,3),( 3,2)),(( 3,2),( 2, 4))],[((1,3),( 3,2)),(( 3,2),( 4, 4))],[((1,3),( 3,2)),(( 3,2),( 5,3))],
                            [((1,3),( 3,2)),(( 3,2),( 5,1))],[((1,3),( 3,2)),(( 3,2),( 4, 0))],[((1,3),( 3,2)),(( 3,2),( 2, 0))],[((1,3),( 3,2)),(( 3,2),( 1,1))],
                            [((1,3),( 2,1)),(( 2,1),( 0,2))],[((1,3),( 2,1)),(( 2,1),( 1, 3))],[((1,3),( 2,1)),(( 2,1),( 3, 3))],[((1,3),( 2,1)),(( 2,1),( 4,2))],
                            [((1,3),( 2,1)),(( 2,1),( 4,0))],[((1,3),( 2,1)),(( 2,1),( 3,-1))],[((1,3),( 2,1)),(( 2,1),( 1,-1))],[((1,3),( 2,1)),(( 2,1),( 0,0))],
                            [((1,3),( 0,1)),(( 0,1),(-2,2))],[((1,3),( 0,1)),(( 0,1),(-1, 3))],[((1,3),( 0,1)),(( 0,1),( 1, 3))],[((1,3),( 0,1)),(( 0,1),( 2,2))],
                            [((1,3),( 0,1)),(( 0,1),( 2,0))],[((1,3),( 0,1)),(( 0,1),( 1,-1))],[((1,3),( 0,1)),(( 0,1),(-1,-1))],[((1,3),( 0,1)),(( 0,1),(-2,0))],
                            [((1,3),(-1,2)),((-1,2),(-3,3))],[((1,3),(-1,2)),((-1,2),(-2, 4))],[((1,3),(-1,2)),((-1,2),( 0, 4))],[((1,3),(-1,2)),((-1,2),( 1,3))],
                            [((1,3),(-1,2)),((-1,2),( 1,1))],[((1,3),(-1,2)),((-1,2),( 0, 0))],[((1,3),(-1,2)),((-1,2),(-2, 0))],[((1,3),(-1,2)),((-1,2),(-3,1))]])
                    $ spgGenerator ((1, 3), 2, (4, 7))


testSpgFilter1 = TestCase $ assertEqual "Test spgFilter with empty moves"
                    ((4, 7), [[]])
                    $ spgFilter ((4, 7), [[]])

testSpgFilter2 = TestCase $ assertEqual "Test spgFilter with invalid moves"
                    ((1, 3), [[((1, 3), (2, 5))], [((1, 3), (3, 4))], [((1, 3), (3, 2))], [((1, 3), (2, 1))]])
                    $ spgFilter ((1, 3), [[((1, 3), (-1, 4))], [((1, 3), (0, 5))], [((1, 3), (2, 5))], [((1, 3), ( 3, 4))],
                                          [((1, 3), ( 3, 2))], [((1, 3), (2, 1))], [((1, 3), (0, 1))], [((1, 3), (-1, 2))]])


testSpgSelektor1 = TestCase $ assertEqual "Test spgSelektor with empty moves"
                    []
                    $ spgSelektor ((4, 7), [[]])

testSpgSelektor2 = TestCase $ assertEqual "Test spgSelektor with valid moves"
                    [[((1, 3), (3, 2))], [((1, 3), (3, 2))]]
                    $ spgSelektor ((3, 2), [[((1, 3), (3, 2))], [((1, 3), (3, 4))], [((1, 3), (3, 2))], [((1, 3), (2, 1))]])


testSpgTransformer = TestCase $ assertEqual "Test spgTransformer"
                    [[((A, Acht), (D, Sieben))], [((A, Drei), (C, Zwei))]]
                    $ spgTransformer [[((1, 8), (4, 7))], [((1, 3), (3, 2))]]


testSpringer1 = TestCase $ assertEqual "Test springer with no moves"
                    []
                    $ springer (D, Vier) 0 (B, Fuenf)

testSpringer2 = TestCase $ assertEqual "Test springer with goal not reached"
                    []
                    $ springer (D, Vier) 1 (B, Acht)

testSpringer3 = TestCase $ assertEqual "Test springer with goal reached once"
                    [[((D, Vier), (B, Fuenf))]]
                    $ springer (D, Vier) 1 (B, Fuenf)

testSpringer4 = TestCase $ assertEqual "Test springer with multiple moves"
                    [[((D,Vier),(B,Fuenf)),((B,Fuenf),(C,Drei)),((C,Drei),(A,Zwei))],[((D,Vier),(C,Sechs)),((C,Sechs),(B,Vier)),((B,Vier),(A,Zwei))],
                     [((D,Vier),(E,Zwei )),((E,Zwei ),(C,Drei)),((C,Drei),(A,Zwei))],[((D,Vier),(E,Zwei )),((E,Zwei ),(C,Eins)),((C,Eins),(A,Zwei))],
                     [((D,Vier),(C,Zwei )),((C,Zwei ),(B,Vier)),((B,Vier),(A,Zwei))],[((D,Vier),(B,Drei )),((B,Drei ),(C,Eins)),((C,Eins),(A,Zwei))]]
                    $ springer (D, Vier) 3 (A, Zwei)