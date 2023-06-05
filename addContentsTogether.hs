addContentsTogether [] = 0
addContentsTogether xs = foldl (\acc x -> x + acc) 0 xs