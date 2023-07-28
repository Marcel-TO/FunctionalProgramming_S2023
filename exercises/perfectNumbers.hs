divisors :: Integer -> [Integer]
divisors n = [x | x <- [1 .. n - 1], mod n x == 0]

isPerfect :: Integer -> Bool
isPerfect n = n == sum(divisors n)

listPerfect :: Integer -> [Integer]
listPerfect 0 = []
listPerfect n = [x | x <- [1..n], isPerfect x]