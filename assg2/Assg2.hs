module Assg2 where

-- Helpers --

-- sieve of Eratosthenes
primes = sieve [2..]
sieve (x:xs) = x : sieve [y | y <- xs, mod y x > 0]

-- Get the decimals of a number in list form
decimals 0 = []
decimals n = decimals (n `div ` 10) ++ [n `mod` 10]

-- Check if a number is in a _sorted_ list
isIn :: Integer -> [Integer] -> Bool
isIn n (x:xs)
    | n > x     = isIn n xs
    | n == x    = True
    | otherwise = False
--------------------------------------------

-- Ex 1
nOfOnesInBinary :: Integer -> Integer
nOfOnesInBinary n
    | n == 0    = 0
    | otherwise = (n `mod` 2) + nOfOnesInBinary(n `div` 2)

isPrime :: Integer -> Bool
isPrime n = n `isIn` primes

ddps :: [Integer]
ddps = [x | x <- [2..], isPrime(nOfOnesInBinary(x))]
--------------------------------------------


-- Ex 2
isPrimeList :: [Bool]
isPrimeList = [isPrime x | x <- [0..]]

ddpsMT :: [Integer]
ddpsMT = [x | x <- [2..], isPrimeList !! fromIntegral(nOfOnesInBinary(x))]
--------------------------------------------


-- Ex 3
pow3List :: [Integer]
pow3List = [powMT x | x <- [0..]]

powMT :: Int -> Integer
powMT 0 = 1
powMT n = (pow3List !! (n-1)) + (pow3List !! (n-1)) + (pow3List !! (n-1))

-- naive implemtantion (bad and also without memoization)
--pow :: Int -> Integer
--pow 0 = 1
--pow n = pow(n-1) + pow(n-1) + pow(n-1)
--------------------------------------------


-- Ex 4

f :: Integer -> Integer -> Double
f x k = foldr (+) 0 (take (fromIntegral k + 1) terms)
    where terms = [h x i | i <- [1,3..]]

-- helper function for f and fMT
h :: Integer -> Integer -> Double
h x i = (fromIntegral x ^ i) / (fromIntegral $ fact i)

-- Factorial function
fact :: Integer -> Integer
fact 0 = 1
fact 1 = 1
fact n = n * fact (n - 1)
-- Alternative with streams: fact = 1 : zipWith (*) [1..] fact
-- results in "ERROR - Garbage collection fails to reclaim sufficient space" for much lower factorial values than the recursive implementation


-- Ex 5

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

