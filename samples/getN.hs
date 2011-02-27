getN 1 (x:xs) = x
getN n (x:xs) = getN (n-1) xs

main = do
	print (getN 1000021 [1, 2 ..])
