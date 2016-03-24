module Assg2 where

-- Helpers --

-- sieve of Eratosthenes
primes = sieve [2..]
sieve (x:xs) = x : sieve [y | y <- xs, mod y x > 0]

-- Get the decimals of a number in list form
decimals 0 = []
decimals n = decimals (n `div ` 10) ++ [n `mod` 10]
--------------------------------------------


-- Ex 4
gz :: Integer -> Integer
gz n
	    | n <= 0 = 0
	    | otherwise = foldl (*) 1 ( zipWith (**) primes ( decimals n ) )

gzs :: [Integer]
gzs = map gz [1..]
--------------------------------------------


