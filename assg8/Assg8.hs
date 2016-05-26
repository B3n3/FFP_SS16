module Assg8 where


simpleMSS :: [Int] -> [Int]
simpleMSS = snd . maximum . zipSum . segments

segments :: [Int] -> [[Int]]
segments [] = []
segments l = l : ((segments $ init l) ++ (segments $ tail l))

zipSum :: [[Int]] -> [(Int, [Int])]
zipSum l = zip (map sum l) l 