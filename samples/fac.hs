fac m 1 = m
fac m n = fac (m * n) (n - 1)

f n = fac 1 n

main = do
	print (take 10 (map f [1, 2 ..]))
--	print (take 10 (map (\n -> n * n) [1, 2 ..]))
--	print (take 10 (map (\n -> n ^ 3) [1, 2 ..]))
--	print (take 10 (map (\n -> 5 ^ n) [1, 2 ..]))
