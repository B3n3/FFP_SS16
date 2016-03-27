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
--  Goedel Zahlen
gz :: Integer -> Integer
gz n
	    | n <= 0 = 0
	    | otherwise = foldl (*) 1 ( zipWith (^) primes ( decimals n ) )

-- Stream of goedle numbers
gzs :: [Integer]
gzs = map gz [1..]

-- goedel numbers with memoization
gzMT :: Integer -> Integer
gzMT n
	    | n <= 0 = 0
	    | otherwise = helpGz (decimals n) 0

-- recursive function to combine (multiply) results from memo table
helpGz :: [Integer] -> Integer -> Integer
helpGz [] _ = 1
helpGz (x:xs) i = ( (gzMTList !! fromIntegral(x)) !! fromIntegral(i) ) * ( helpGz xs (i+1))

-- memo list of primes
gzMTList = [
            [1,1..],
            primes,
            map (^2) primes,
            map (^3) primes,
            map (^4) primes,
            map (^5) primes,
            map (^6) primes,
            map (^7) primes,
            map (^8) primes,
            map (^9) primes
           ]

-- stream of goedel numbers with memoization
gzsMT :: [Integer]
gzsMT = map gzMT [1..]
--------------------------------------------

