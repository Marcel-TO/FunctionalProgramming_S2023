import Data.List
import Data.Char

-- Ceasar Cipher (The number is the offset for the encryption/decryption)
ceasar_encrypt :: String -> Int -> String
ceasar_encrypt [] _ = []
ceasar_encrypt (x:xs) n = ceasar_shift x n : ceasar_encrypt xs n

ceasar_shift :: Char -> Int -> Char
ceasar_shift c n
    | (ord c) + n >= 127 = chr (((ord c) + n) - 95) -- 127 - 32
    | c == ' ' = c -- Checks for space
    | otherwise = chr ((ord c) + n)

ceasar_decrypt :: String -> Int -> String
ceasar_decrypt xs n = ceasar_encrypt xs (negate n)


-- Quick Sort
myquicksort :: Ord a => [a] -> [a]
myquicksort [] = []
myquicksort (x:xs) = myquicksort (filter (< x) (x:xs)) ++ [x] ++ myquicksort (filter (> x) (x:xs))


-- reverse Polish Notation (RPN)
rpn :: String -> Integer
rpn [] = 0
rpn expr = head $ foldl rpnEval [] (words expr)

rpnEval :: [Integer] -> String -> [Integer]
rpnEval (x:y:xs) c
    | c == "+" = (x+y) : xs
    | c == "-" = (x-y) : xs
    | c == "*" = (x*y) : xs
    | c == "/" = div x y : xs 
rpnEval xs numStr = read numStr : xs