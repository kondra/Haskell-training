pali [] = []
pali xs = xs ++ (rev xs)
        where rev [] = []
              rev (x:xs) = (rev xs) ++ [x]
