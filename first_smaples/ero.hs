ero (x : xs) = x : ero [y | y <- xs, (rem y x) /= 0]
main = print (take 100 (ero [2..]))
