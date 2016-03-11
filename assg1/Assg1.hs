module Assg1 where

powersOfThrees :: [Integer]
powersOfThrees = 1 : map (*3) powersOfThrees


sd :: [[Integer]]
sd = [1] : map computeNextStirlingTupel sd

computeNextStirlingTupel :: [Integer] -> [Integer]
computeNextStirlingTupel current = 1 : (zipWith (+) current $ zipWith (*) [2..] $ tail current) ++ [1]


bn :: [Integer]
bn = map (foldr (+) 0) sd