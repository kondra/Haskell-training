oddEven [] = []
oddEven (x:y:xs) = y : x : oddEven xs

main = do
	print (oddEven [1, 2 .. 20])
