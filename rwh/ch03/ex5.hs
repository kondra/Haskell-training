isPali [] = True
isPali xs 
          | length xs == 1 = True
          | (head xs) == (last xs) = isPali (tail (take (length xs - 1) xs))
		  | otherwise = False
