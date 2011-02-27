divisors n = [ x | x <- [1..n], rem n x == 0 ]

primes = [x | x <- [2..], divisors x == [1, x] ]

--main = do
--  c <- getLine
--  print (take (read c) primes)
main = print (take 1000 primes)
