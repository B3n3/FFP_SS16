module Assg6 where

data Token = Wrong | Mult | Add | Value Integer deriving (Eq,Show)

--Ex 1

inOut :: (String -> String) -> IO Integer
inOut f = do
    line <- getLine
    if line == "end"
        then return 0
        else do
            print $ f line
            nLines <- inOut f
            return $ 1 + nLines

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

interpret :: [Token] -> [Integer]
interpret t = interpret' t []
    where
        interpret' [] stack = stack
        interpret' (t:r) stack = case t of
            Wrong -> []
            Mult -> if length stack < 2
                then []
                else interpret' r $ ((stack !! 0) * (stack !! 1)) : drop 2 stack
            Add -> if length stack < 2
                then []
                else interpret' r $ ((stack !! 0) + (stack !! 1)) : drop 2 stack
            Value n -> interpret' r $ n : stack


--Ex 5

main :: IO ()
main = do
    nLines <- inOut (toString . interpret . tokenize)
    print nLines