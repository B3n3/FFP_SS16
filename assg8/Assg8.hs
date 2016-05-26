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

outCamp :: Camp -> [[Char]]
outCamp = group8 . map tochr . elems
    where 
        tochr Tree = 'B'
        tochr Tent = 'Z'
        tochr Empty = 'u'
        group8 [] = []
        group8 lst = (take 8 lst) : (group8 $ drop 8 lst)