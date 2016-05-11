module Assg7 where


type Text = String
type Word = String
type First = Int
type Last = Int

occS :: Text -> Assg7.Word -> [(First, Last)]
occS text word = occS' (length word - 1) (length word - 1) word text

occS' :: Last -> First -> Assg7.Word -> Text -> [(First, Last)]
occS' last first word text
    | last >= length text   = []
    | last - first == length word
        = (first+1, last) : occS' (last + length word) (last + length word) word text
    | text !! first == word !! (length word - (last - first + 1))
        = occS' last (first - 1) word text
    | otherwise             = occS' (last + 1) (last + 1) word text


occI :: Text -> Assg7.Word -> [(First, Last)]
occI text word = occI' (length word - 1) (length word - 1) word text

occI' :: Last -> First -> Assg7.Word -> Text -> [(First, Last)]
occI' last first word text
    | last >= length text   = []
    | last - first == length word
        = (first+1, last) : occI' (last + length word) (last + length word) word text
    | text !! first  == word !! (length word - (last - first + 1))
        = occI' last (first-1) word text
    | otherwise = occI' (last + (skip (text !! first) word)) (last + (skip (text !! first) word)) word text


-- Pseudocode:
--
--function preprocess(pattern)
--    T ← new table of 256 integers
--    for i from 0 to 256 exclusive
--        T[i] ← length(pattern)
--    for i from 0 to length(pattern) - 1 exclusive
--        T[pattern[i]] ← length(pattern) - 1 - i
--    return T


preprocess :: Word -> [(Char, Int)]
preprocess w = preprocess' w 0 (length w)

preprocess' :: [Char] -> Int -> Int ->  [(Char, Int)]
preprocess' [] i len = []
preprocess' (x:xs) i len
    | i == 0        = [(x, len-1)] ++ preprocess' xs (i+1) len
    | i == len -1   = [(x, len)]
    | otherwise     = [(x, len-i-1)] ++ preprocess' xs (i+1) len

getSkip :: Char -> [(Char,Int)] -> Int -> Int
getSkip _ [] len = len
getSkip c (x:xs) len
    | c == fst x    = snd x
    | otherwise     = getSkip c xs len

skip :: Char -> [Char] -> Int
skip c w = getSkip c (reverse (preprocess w)) (length w)


prop_coincide :: Text -> Word -> Bool
prop_coincide t w = occS t w == occI t w