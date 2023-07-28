-- isPal: Test auf Palindrom (Variante ohne Verwendung von reverse)

isPal :: (Eq a) => [a] -> Bool
isPal xs = xs == reverse xs

isPalWithoutReverse xs = xs == foldl (flip ( : )) [] xs