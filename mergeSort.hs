-- [2,5,6] + [1,3,4] -> [1,2,3,4,5,6]
mergeSort :: Ord a => [a] -> [a] -> [a]
mergeSort [] [] = []
mergeSort as bs = myquicksort (as ++ bs)

myquicksort :: Ord a => [a] -> [a]
myquicksort [] = []
myquicksort (x:xs) = myquicksort (filter (< x) (x:xs)) ++ [x] ++ myquicksort (filter (> x) (x:xs))