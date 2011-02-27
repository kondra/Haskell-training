--inc :: Integer -> Integer
inc x = x + 1

main = do
  c <- getLine
  inc (read c)
