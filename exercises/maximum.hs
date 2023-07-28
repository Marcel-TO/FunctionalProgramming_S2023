f 0 = 0
f x = 1 + f(x - 1)

mysum [] = 0
mysum (x:xs) = x + mysum xs

mymax x y = if x < y then y else x


mymaximum [] = error "will nicht"
mymaximum [x] = x
mymaximum (x:xs) = mymax x (mymaximum xs)