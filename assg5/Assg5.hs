module Assg5 where

import Data.Array

-- Ex 1

mas :: Array Int Int -> Int
mas a = maximum ( map snd ( sumsOfSection a ) )


-- helper to get all possible sections
sections :: Ix a => Array a b -> [(a,a)]
sections a = [(x,y) | x <- (indices a), y <- (indices a), x<=y]

-- helper to calculate the sum of one section
sectionSum :: (Ix a, Num b) => Array a b -> (a,a) -> b
sectionSum a section = sum ( map (a !) ( range section ) )

-- helper to get a list of pairs of   section-range + the sum of it
sumsOfSection :: (Ix a, Num b) => Array a b -> [((a,a),b)]
sumsOfSection a = map (\index -> (index,sectionSum a index)) $ sections a

-----------------------------


-- Ex 2

amas :: Array Int Int -> [(Int,Int)]
-- compare each sum with the result of mas, with filter only the matching
-- elements get selected (fst takes the first element which is the index)
amas a = map fst ( filter (\(_,max) -> max == mas a) (sumsOfSection a)  )

-----------------------------


-- Ex 3

lmas :: Array Int Int -> (Int, Int)
lmas lst = head $ filter (\(start, end) -> end - start == maxDiff) amasArray
    where maxDiff = maximum $ map (\x -> snd x - fst x) amasArray
          amasArray = amas lst

-----------------------------

-- Ex 4

minIndex :: (Ix a, Show a) => Array a b -> (b -> Bool) -> a
minIndex array check
    | length result == 0 = error "No matching index"
    | otherwise = head result
        where
            result = divideAndConquer mi_indiv mi_solve mi_divide mi_combine $ assocs array

            mi_indiv lst = length lst <= 1

            mi_solve [] = []
            mi_solve ((a, b):_)
                | check b = [a]
                | otherwise = []

            mi_divide (l@(a, _):ls) = [filter (\x -> a >  fst x) ls, 
                                       [l],
                                       filter (\x -> a <= fst x) ls]

            mi_combine _ [l1, l2, l3] = l1 ++ l2 ++ l3

divideAndConquer :: (p -> Bool) -> (p -> s) -> (p -> [p]) -> (p -> [s] -> s) -> p -> s
divideAndConquer indiv solve divide combine initPb
    = dAC initPb
        where
            dAC pb
                | indiv pb = solve pb
                | otherwise = combine pb (map dAC (divide pb))