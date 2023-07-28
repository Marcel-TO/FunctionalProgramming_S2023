-- [1,0,1,0]
binaryToDec [] = 0
binaryToDec (x:xs) = (x *2 ^ (length xs)) + binaryToDec xs