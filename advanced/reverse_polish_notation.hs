-- reverse Polish Notation (RPN)
rpn :: String -> Integer
rpn [] = 0
rpn expr = head $ foldl rpnEval [] (words expr)

rpnEval :: [Integer] -> String -> [Integer]
rpnEval (x:y:xs) c
    | c == "+" = (x+y) : xs
    | c == "-" = (x-y) : xs
    | c == "*" = (x*y) : xs
    | c == "/" = div x y : xs 
rpnEval xs numStr = read numStr : xs