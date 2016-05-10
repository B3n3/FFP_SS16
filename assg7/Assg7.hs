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
    | otherwise = []
