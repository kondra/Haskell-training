myLength [] = 0
myLength (x:xs) = 1 + myLength xs

mySum [] = 0
mySum (x:xs) = x + mySum xs

mean [] = 0
mean xs = (mySum xs) / (myLength xs)
