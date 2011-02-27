factor 1 = [] 
factor n = x : factor (quot n x)
  where x = head [ y | y <- [2..n], rem n y == 0]

main = do
  --c <- getLine
  print (factor 23423566)
