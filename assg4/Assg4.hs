module Assg4 where

import Array


-- Ex 2 binomDyn
------------------
-- binomDyn is faster than binomM, but my implementation of binomS is still way faster :)
-- I think this is because I exploited the correlation between the binomial coefficient and
-- the pascal triangle in a very efficient way.
-- By simply using dynamic programming, this knowledge is never used and therefore
-- I don't think it could ever become that fast.

binomDyn :: (Integer,Integer) -> Integer
binomDyn (n,k) = findTable t (n,k)
	where t = dynamic compB (bndsB (n,k))

-- simple bound function for binomDyn
bndsB :: (Integer,Integer) -> ((Integer,Integer),(Integer,Integer))
bndsB (n,k) = ((0,0),(n,k))

-- compare function takes table and (n,k), returns integer
-- uses the table to combine results
compB :: Table Integer (Integer,Integer) -> (Integer,Integer) -> Integer
compB t (n,k)
	| 0 < k && k < n = findTable t (n-1, k-1) + findTable t (n-1, k)
	| k == 0 || k == n = 1
	| otherwise = 0


-- Dynamic function for higher order functions of the lecture slides
dynamic :: (Ix coord) => (Table entry coord -> coord -> entry) -> (coord,coord) -> (Table entry coord)
dynamic compute bnds = t
	where t = newTable (map (\coord -> (coord,compute t coord)) (range bnds))


-- Table data structure from the lecture slides
newtype Table a b = Tbl (Array b a)

newTable :: (Ix b) => [(b,a)] -> Table a b
newTable l = Tbl (array (lo,hi) l)
	where
		indices = map fst l
		lo = minimum indices
		hi = maximum indices

findTable :: (Ix b) => Table a b -> b -> a
findTable (Tbl a) i = a ! i

updTable :: (Ix b) => (b,a) -> Table a b -> Table a b
updTable p@(i,x) (Tbl a) = Tbl (a // [p])

