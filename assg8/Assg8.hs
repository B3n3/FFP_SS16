module Assg8 where

import Data.Array

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


data Content = Tree | Tent | Empty deriving (Eq,Ord,Show)
type Camp = Array (Int,Int) Content
type Row = Int -- ausschliesslich Werte von 1 bis 8
type Column = Int -- ausschliesslich Werte von 1 bis 8
type LocationsOfTrees = [(Row,Column)]
type TentsPerRow = [Int] -- Liste der Laenge 8, ausschliesslich Werte von 0 bis 4; Wert des i-ten Elements bezeichnet Zahl der Zelte in Reihe i
type TentsPerColumn = [Int] -- Liste der Laenge 8, ausschliesslich Werte von 0 bis 4; Wert des j-ten Elements bezeichnet Zahl der Zelte in Spalte j
type Solution = (Camp, TentsPerRow, TentsPerColumn)

outCamp :: Camp -> [[Char]]
outCamp = group8 . map tochr . elems
    where 
        tochr Tree = 'B'
        tochr Tent = 'Z'
        tochr Empty = 'u'
        group8 [] = []
        group8 lst = (take 8 lst) : (group8 $ drop 8 lst)


simpleCamp :: LocationsOfTrees -> TentsPerRow -> TentsPerColumn -> Camp
simpleCamp lot tpr tpc = maybe (makeCamp lot) id result
    where
        result = findFirstValid tpr tpc $ filter validPositions $ expandSolutions $ makeCamp lot

findFirstValid :: TentsPerRow -> TentsPerColumn -> [Camp] -> Maybe Camp
findFirstValid _ _ [] = Nothing
findFirstValid tpr tpc (camp:xs) = if validTentsNumber then (Just camp) else (findFirstValid tpr tpc xs)
        where
            validTentsNumber = (all rowHasValidNumber [1..8]) && (all colHasValidNumber [1..8])
            rowHasValidNumber row = (tpr !! (row - 1)) == (length $ filter (\x -> fst (fst x) == row) (assocs camp))
            colHasValidNumber col = (tpc !! (col - 1)) == (length $ filter (\x -> snd (fst x) == col) (assocs camp))


validPositions :: Camp -> Bool
validPositions camp = (all treeHasTent $ content) && (not $ any tentNearTent $ content)
    where
        content = assocs camp
        directions8 = [(-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1)]
        neighbours8 point = map (\x -> (fst point + fst x, snd point + snd x)) directions8
        neighbours4 point = map (\x -> (fst point + fst x, snd point + snd x)) (filter (\x -> fst x == 0 || snd x == 0) directions8)
        treeHasTent ((i, j), Tree) = any (\x -> fst x > 0 && fst x < 8 && snd x > 0 && snd x < 8 && camp ! x == Tent) $ neighbours4 (i, j)
        treeHasTent _ = True
        tentNearTent ((i, j), Tent) = any (\x -> fst x > 0 && fst x < 8 && snd x > 0 && snd x < 8 && camp ! x == Tent) $ neighbours8 (i, j)
        tentNearTent _ = False

makeCamp :: LocationsOfTrees -> Camp
makeCamp treeLocations = listArray ((1,1),(8,8)) campElemns
    where
        campElemns = map (\x -> if (elem x treeLocations) then Tree else Empty) positions
        positions = [(i, j) | i <- [1..8], j <- [1..8]]

expandSolutions :: Camp -> [Camp]
expandSolutions camp = map (\x -> listArray ((1,1),(8,8)) x) choices
    where
        choices  = cp choices'
        choices' = map (\x -> if (x  == Empty) then [Empty, Tent] else [Tree]) $ elems camp
        cp [] = [[]]
        cp (xs:xss) = [x:ys | x <- xs, ys <- cp xss]