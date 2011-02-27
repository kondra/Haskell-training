factor b 1 = [] 
factor b n = x : factor x (quot n x)
  where x = head [ y | y <- [b..n], rem n y == 0]

main = do
  --c <- getLine
  print (factor 2 23423566)
