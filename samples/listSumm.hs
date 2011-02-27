listSumm [] ys = ys
listSumm xs [] = xs
listSumm (x:xs) (y:ys) = (x + y) : listSumm xs ys

main = do
	print (listSumm [1, 2 .. 10] [1, 2 .. 100])
