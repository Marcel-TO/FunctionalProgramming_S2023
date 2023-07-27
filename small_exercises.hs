import Data.List

-- doubleReverse: abcd -> abcddcba
doubleReverse :: [Integer] -> [Integer]
doubleReverse x = x ++ reverse x

-- compr: Kompression "aaabbbcccca" -> [(3,'a'), (3,'b'), (4,'c'), (1,'a')]
compr :: Eq a => [a] -> [(a, Int)]
compr = map (\y -> (head y, length y)) . group

-- ggt: Größter gemeinsamer Teiler
ggt :: Integer -> Integer -> Integer
ggt x 0 = x
ggt x y = ggt y (mod x y)

-- factors
factors :: Integer -> [Integer]
factors n = [x | x <- [1 .. n], mod n x == 0]
-- filterAdjacent: Filter einer Liste, wobei Element nur erhalten bleibt, wenn ident mit nachfolgendem, "abbbccd" -> "bbc"
-- isPal: Test auf Palindrom (Variante ohne Verwendung von reverse)
-- isPrime: Test auf Primzahl
-- isPowOfTwo: Test ob Zahl Potenz von 2 ist.
-- kgv: Kleinstes gemeinsames Vielfaches
-- listPrime: Zwei Zahlen als Eingabe, alle Primzahlen dazwischen als Ausgabe
-- primFaktorZerlegung: Ausgabe Liste von Tupel mit (Faktor, Anzahl), 144 -> [(2,4),(3,2)]
-- sums: [Int] -> [Int], n-te Zahl der Ausgabe = Summe der letzten n Zahlen der (endlichen) Eingabe, [1..5] -> [5,9,12,14,15]
