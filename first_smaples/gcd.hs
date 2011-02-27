--gcd1 :: Integer -> Integer -> Integer
gcd1 x 0 = x
gcd1 x y = gcd1 y (rem x y)

lcm1 x 0 = 0
lcm1 0 x = 0
lcm1 x y = quot (x * y) (gcd x y)

main = do
  s1 <- getLine
  s2 <- getLine
  print (gcd1 (read s1) (read s2))
  print (lcm1 (read s1) (read s2))
