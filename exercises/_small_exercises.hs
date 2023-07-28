import Data.List

-- doubleReverse: abcd -> abcddcba
doubleReverse :: [Integer] -> [Integer]
doubleReverse x = x ++ reverse x

-- compr: Kompression "aaabbbcccca" -> [(3,'a'), (3,'b'), (4,'c'), (1,'a')]
compr :: Eq a => [a] -> [(a, Int)]
compr = map (\y -> (head y, length y)) . group

-- ggt: Größter gemeinsamer Teiler
ggt :: Integer -> Integer -> Integer
ggt x 0 = abs x
ggt x y = ggt y (mod x y)

-- ggtList: Größter gemeinsamer Teiler von einer Liste
ggtList :: [Integer] -> Integer
ggtList [] = 0
ggtList (x:xs) = foldl ggt x xs

-- factors
factors :: Integer -> [Integer]
factors n = [x | x <- [2 .. n-1], mod n x == 0]

-- filterAdjacent: Filter einer Liste, wobei Element nur erhalten bleibt, wenn ident mit nachfolgendem, "abbbccd" -> "bbc"
filterAdjacent :: String -> String
filterAdjacent = concatMap init . group

-- isPal: Test auf Palindrom (Variante ohne Verwendung von reverse)
isPal :: Eq a => [a] -> Bool
isPal x = x == reverse x


-- isPrime: Test auf Primzahl
isPrime :: Integer -> Bool
isPrime n = n > 1 && null (factors n)


-- isPowOfTwo: Test ob Zahl Potenz von 2 ist.
isPowOfTwo :: Integer -> Bool
isPowOfTwo 1 = True
isPowOfTwo n
    | n < 1 = False
    | mod n 2 /= 0 = False
    | n >= 2  = isPowOfTwo (div n 2)

-- kgv: Kleinstes gemeinsames Vielfaches
kgv :: Integer -> Integer -> Integer
kgv x 0 = abs x
kgv 0 y = abs y
kgv x y = div (abs x * y) (ggt x y)

-- Error, wieso auch immer kommt nicht das richtige heraus.....
-- kgvList: Kleinstes gemeinsames Vielfaches einer Liste
kgvList :: [Integer] -> Integer
kgvList [] = 1
kgvList (x:xs) = foldl kgv x xs 

-- listPrime: Zwei Zahlen als Eingabe, alle Primzahlen dazwischen als Ausgabe
listPrime :: Integer -> Integer -> [Integer]
listPrime x y = if x > y then listPrime y x else [a | a <- [x .. y], isPrime a]

-- primFaktorZerlegung: Ausgabe Liste von Tupel mit (Faktor, Anzahl), 144 -> [(2,4),(3,2)]

-- sums: [Int] -> [Int], n-te Zahl der Ausgabe = Summe der letzten n Zahlen der (endlichen) Eingabe, [1..5] -> [5,9,12,14,15]
sums :: [Int] -> [Int]
sums = scanl1 (+) . reverse

