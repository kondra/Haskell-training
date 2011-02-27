-- problem1
myLast :: [a] -> a
myLast [] = error "empty list"
myLast (x:xs)
    | null xs = x
    | otherwise = myLast xs

--v2
myLast' :: [a] -> a
myLast' [] = error "empty list"
myLast' [x] = x
myLast' (_:xs) = myLast' xs

--v3
myLast'' :: [a] -> a
myLast'' = head . reverse

--problem2
myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

--v2
myButLast' :: [a] -> a
myButLast' = head . tail . reverse

--problem3
elementAt :: [a] -> Integer -> a
elementAt [] _ = error "index out of range"
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n - 1)

--problem4
myLength :: [a] -> Integer
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

--v2 tail recursion
myLength' :: [a] -> Integer
myLength' = myLength'' 0
        where myLength'' n [] = n
              myLength'' n (_:xs) = myLength'' (n + 1) xs

--problem5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

--v2 tail recursion
myReverse' :: [a] -> [a]
myReverse' = myReverse'' []
        where myReverse'' rev [] = rev
              myReverse'' rev (x:xs) = myReverse'' ([x] ++ rev) xs

--problem6
isPalindrome [] = True
isPalindrome [_] = True 
isPalindrome (x:xs) = (x == (last xs)) && (isPalindrome $ init xs)

--problem7
data MyList a = Elem a 
              | List [MyList a]
              deriving (Show)

flatten (List []) = []
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

--problem8
compress [] = []
compress (x:xs) = x : compress' xs x
                where compress' [] _ = []
                      compress' (x:xs) y | x == y = compress' xs x
                                         | otherwise = x : compress' xs x

--problem9
pack [] = []
pack l@(x:xs) = [first] ++ (pack $ drop len l)
              where first = pack' l x
                    len = length first
                    pack' [] _ = []
                    pack' (x:xs) y | x == y = x : pack' xs x
                                   | otherwise = []

--problem10
encode [] = []
encode l@(x:xs) = (len, x) : (encode $ drop len l)
              where len = encode' l x
                    encode' [] _ = 0
                    encode' (x:xs) y | x == y = 1 + encode' xs x
                                     | otherwise = 0


--problem 11
data MyPair = Single Char
| Multiple Int Char
deriving (Show)

group [] = []
group xs@(x:_) = begin : group end
where begin = takeWhile ((==) x) xs
      end = dropWhile ((==) x) xs

encodeModified xs = map cons $ group xs
where cons [x] = Single x
      cons xs@(x:_) = Multiple (length xs) x
