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
