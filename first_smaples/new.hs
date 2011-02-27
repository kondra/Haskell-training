--gcd :: Integer -> Integer -> Integer
--gcd x 0 = x
--gcd x y = gcd y (rem x y)

--primes :: 
relprimes = [ (x, y) | x <- [1..], y <- [1..x], (gcd x y) == 1 ]

sq = [ x^2 | x <- [1..] ]

main = print (take 10 sq)
