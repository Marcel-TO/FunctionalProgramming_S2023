import Data.List

-- compr: Kompression "aaabbbcccca" -> [(3,'a'), (3,'b'), (4,'c'), (1,'a')]
comprSlow :: (Eq a) => [a] -> [(Int, a)]
comprSlow [] = []
comprSlow xs = zip ints chars
    where ints = map length (group xs)
          chars = map head (group xs)

-- compr: Kompression "aaabbbcccca" -> [(3,'a'), (3,'b'), (4,'c'), (1,'a')]
compr :: (Eq a) => [a] -> [(Int, a)]
compr = map (\x -> (length x, head x)) . group