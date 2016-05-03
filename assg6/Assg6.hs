module Assg6 where

data Token = Wrong | Mult | Add | Value Integer deriving (Eq,Show)

--Ex 1

inOut :: (String -> String) -> IO Integer
inOut f = do
    lines <- readUntilEnd
    sequence $ map (print . f) lines
    return $ fromIntegral $ length lines

readUntilEnd :: IO [String]
readUntilEnd = do
    line <- getLine
    if line == "end"
        then return []
        else do
            lines <- readUntilEnd
            return $ line:lines

--Ex 2

tokenize :: String -> [Token]
tokenize "" = []
tokenize s@(c:r)
    | elem c whitespaceChars = tokenize r 
    | c == '*' = Mult : tokenize r
    | c == '+' = Add : tokenize r
    | elem c valueChars = (parseValue s) : (tokenize $ nextAfterValue s)
    | otherwise = Wrong : tokenize r
    where
        whitespaceChars = " \t"
        valueChars = "-0123456789"
        parseValue s = Value $ read $ takeWhile (\c -> elem c valueChars) s
        nextAfterValue s = dropWhile (\c -> elem c valueChars) s


--Ex 3

toString :: [Integer] -> String
toString [] = "Error"
toString lst = joinToString $ map (show) $ reverse lst
    where
        joinToString (h:l) = h ++ foldl (\t c -> t ++ " " ++ c) "" l


--Ex 4


--Ex 5

