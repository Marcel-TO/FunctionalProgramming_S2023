-- übernimmt eine Liste -> berechnet eine Zahl 
listToWhole [] = 0
listToWhole [x] = x
listToWhole (x:xs) = x * (10 ^ length(xs)) + listToWhole xs