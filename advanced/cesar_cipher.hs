import Data.Char

-- 32 >= n < 127

-- Ceasar Cipher (The number is the offset for the encryption/decryption)
ceasarEncrypt :: String -> Int -> String
ceasarEncrypt [] _ = []
ceasarEncrypt (x:xs) n = ceasarShift x n : ceasarEncrypt xs n

ceasarShift :: Char -> Int -> Char
ceasarShift c n
    | ord c + n >= 127 = chr ((ord c + n) - 95) -- 127 - 32
    | c == ' ' = c -- Checks for space
    | otherwise = chr (ord c + n)

ceasarDecrypt :: String -> Int -> String
ceasarDecrypt xs n = ceasarEncrypt xs (negate n)




-- encrypt "ein geheimer satz" 2
encrypt :: [Char] -> Int -> String
encrypt []  x = []
encrypt (x:xs) n = shift x n : encrypt xs n

-- decrypt "ein verschlüsselter satz" 2
decrypt :: [Char] -> Int -> String
decrypt x n = encrypt x (negate n)

-- shift 'a' 2 = 'c'
shift :: Char -> Int -> Char
shift x n = chr ((ord x + n) `mod` 127) -- 127 weil es 127 ASCII characters gibt

mapShift n x = chr ((ord x + n) `mod` 127) -- 127 weil es 127 ASCII characters gibt

mapEncrypt [] n = []
mapEncrypt x n = map (mapShift n) x
mapDecrypt x n = map (mapShift (negate n)) x