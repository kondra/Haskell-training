fact 0 = 1
fact x = x * fact (x-1)

main = do
  c <- getLine
  print (fact (read c))
