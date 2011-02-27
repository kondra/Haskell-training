map1 f [] = []
map1 f (x:xs) = f x : map1 f xs

main = do
	print (map1 (\x -> x * x) [1, 2 .. 10])
