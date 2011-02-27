length1 [] = 0
length1 (x:xs) = 1 + length1 xs

main = do
  s <- getLine
  print (length1 s)
