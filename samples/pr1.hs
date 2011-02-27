add :: Integer -> Integer
add x y = x + y

main = do
  c <- getLine
  x1 <- reads c
  x2 <- reads c
  print (add x1 x2)
