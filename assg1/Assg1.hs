module Assg1 where

-- Ex 1
powersOfThrees :: [Integer]
powersOfThrees = 1 : map (*3) powersOfThrees
--------------------------------------------


-- Ex 2
sd :: [[Integer]]
sd = [1] : map computeNextStirlingTupel sd

computeNextStirlingTupel :: [Integer] -> [Integer]
computeNextStirlingTupel current = 1 : (zipWith (+) current $ zipWith (*) [2..] $ tail current) ++ [1]
--------------------------------------------


-- Ex 3
bn :: [Integer]
bn = map (foldr (+) 0) sd
--------------------------------------------


-- Ex 4
isSquare n =
    sq * sq == n
        where sq = floor $ sqrt $ (fromIntegral n::Double)

squareOf n = floor(sqrt(fromIntegral n))

pt :: [(Integer,Integer,Integer)]
pt = [(a,b,c)| c <- [1..], a <- [1..c-1], let b' = (c*c) - (a*a), isSquare b', let b = squareOf b', a < b]

-- Alternative solution:
--pt = [(x,y,z) | z<-[1..], x<-[1..z], y<-[1..z], x^2+y^2==z^2, x<y ]
--------------------------------------------