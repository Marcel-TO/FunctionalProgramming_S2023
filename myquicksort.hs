myquicksort [] = []
myquicksort (x:xs) = myquicksort (filter (< x) (x:xs)) ++ [x] ++ myquicksort (filter (> x) (x:xs))