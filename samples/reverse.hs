reverse1 [] = []
reverse1 (x:xs) = (reverse1 xs) ++ [x]

main = do
	print (reverse1 [1, 2 .. 20])
