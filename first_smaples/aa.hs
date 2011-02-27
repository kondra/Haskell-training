--rInt :: String -> Int
--rInt c = read c

inc = add 1

add x y = x + y

main = do
  c1 <- getLine
  c2 <- getLine
  print (inc (add (read c1) (read c2)))
