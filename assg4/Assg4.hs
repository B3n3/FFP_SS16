module Assg4 where

import Data.Array
import Data.Sequence
import Data.Foldable

-- Ex 1

searchBfs :: (node -> [node]) -> (node -> Bool) -> node -> [node]
searchBfs succ goal x
    = toList (search' (singleton x))
        where
        search' s
            | Data.Sequence.null s = []
            | goal ((toList $ Data.Sequence.take 1 s) !! 0) = (toList $ Data.Sequence.take 1 s) !! 0 : search' (Data.Sequence.drop 1 s)
            | otherwise =
                let x = (toList $ Data.Sequence.take 1 s) !! 0
                in search' (foldl (|>) (Data.Sequence.drop 1 s) (succ x))

-----------------------------

-- Ex 2

searchDfs succ goal x
    = (search' [x])
        where
            search' s
                | Prelude.null s = []
                | goal (s !! 0) = s !! 0 : search' (Prelude.drop 1 s)
                | otherwise
                    = let x = s !! 0
                    in search' (foldr (:) (Prelude.drop 1 s) (succ x))

data Reihe = A | B | C | D | E | F | G | H deriving (Eq, Ord, Enum, Show)
data Linie = Eins | Zwei | Drei | Vier | Fuenf | Sechs | Sieben | Acht deriving (Eq, Ord, Enum, Show)

type SpgNode = (StartPosition,  ZielPosition, AnzahlZuege, AktPosition, VerbrauchteZuege, Zugfolge)
type StartPosition = (Reihe, Linie)
type ZielPosition = (Reihe, Linie)
type AktPosition = (Reihe, Linie)
type VonPosition = (Reihe, Linie)
type NachPosition = (Reihe, Linie)
type AnzahlZuege = Int
type VerbrauchteZuege = Int
type Zug = (VonPosition, NachPosition)
type Zugfolge = [Zug]

type VirtualPosition = (Int, Int)

getZugfolgeReversed :: SpgNode -> Zugfolge
getZugfolgeReversed (startP,zielP,anzahlZ,aktP,verbrauchteZ, zugfolge) = Prelude.reverse zugfolge

toVirtual :: AktPosition -> VirtualPosition
toVirtual (sr, sl) =
    let convert e = fromIntegral $ fromEnum e
    in (convert sr, convert sl)

fromVirtual :: VirtualPosition -> AktPosition
fromVirtual (row, line) = (toEnum (row), toEnum(line))

genSucc :: VirtualPosition -> [VirtualPosition]
genSucc (row, line) = Prelude.filter spgValidPosition
    [
        (row - 1, line + 2),
        (row + 1, line + 2),
        (row + 2, line + 1),
        (row + 2, line - 1),
        (row + 1, line - 2),
        (row - 1, line - 2),
        (row - 2, line - 1),
        (row - 2, line + 1)
    ]
    where spgValidPosition (row, line) = 0 <= row && row <= 7 && 0 <= line && line <= 7

spgSucc :: SpgNode -> [SpgNode]
spgSucc (startP,zielP,anzahlZ,aktP,verbrauchteZ, zugfolge)
    | verbrauchteZ == anzahlZ       = []
    | verbrauchteZ < anzahlZ        = map f $ map fromVirtual (genSucc $ toVirtual aktP)
        where f newPos = (startP, zielP, anzahlZ, newPos, verbrauchteZ + 1, (aktP, newPos) : zugfolge)

spgGoal :: SpgNode -> Bool
spgGoal (startP,zielP,anzahlZ,aktP,verbrauchteZ, zugfolge)
    | aktP == zielP     = True
    | otherwise         = False

spgBfs :: StartPosition -> ZielPosition -> AnzahlZuege -> [Zugfolge]
spgBfs startP zielP anzahlZ = map (\x -> getZugfolgeReversed x) results
    where results = searchBfs spgSucc spgGoal (startP, zielP, anzahlZ, startP, 0, [])

spgDfs :: StartPosition -> ZielPosition -> AnzahlZuege -> [Zugfolge]
spgDfs startP zielP anzahlZ = map (\x -> getZugfolgeReversed x) results
    where results = searchDfs spgSucc spgGoal (startP, zielP, anzahlZ, startP, 0, [])


-----------------------------

-- Ex 3 binomDyn
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

