import Data.Char

-- 32 >= n < 127

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