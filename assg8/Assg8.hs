module Assg8 where


simpleMSS :: [Int] -> [Int]
simpleMSS = snd . maximum . zipSum . segments

segments :: [Int] -> [[Int]]
segments [] = []
segments l = l : ((segments $ init l) ++ (segments $ tail l))

zipSum :: [[Int]] -> [(Int, [Int])]
zipSum l = zip (map sum l) l

smartMSS :: [Int] -> [Int]
smartMSS = fourth . foldr step (0,0,[],[])
    where 
        fourth (a,b,c,d) = d
        step x (prefixsum,maxsum,maxprefix,maxsegment)
            | x + prefixsum > maxsum = (x + prefixsum, x + prefixsum, x : maxprefix, x : maxprefix)
            | x + prefixsum >= 0 = (x + prefixsum, maxsum, x : maxprefix, maxsegment)
            | otherwise = (0, maxsum, [], maxsegment)