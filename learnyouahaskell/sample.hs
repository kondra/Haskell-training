import Data.Char (isDigit, digitToInt)
import Data.List (foldl', groupBy)

myFoldl f z xs = foldr step id xs z
    where step x g a = g (f a x)

--p97 Ex1
asInt (x:xs) = if x == '-'
             then -1 * res xs
             else res xs
             where res xs = foldl step 0 xs
                   step acc x = acc * 10 + digitToInt x



helper xs = foldl step 0 xs
    where step acc x | isDigit x == True = acc * 10 + digitToInt x
                     | otherwise = error $ "non-digit " ++ [x] 


helper' xs = foldl step (Right 0) xs
    where step (Right acc) x | isDigit x == True = Right $ acc * 10 + digitToInt x
                             | otherwise = Left $ "non-digit " ++ [x] 
          step x _ = x

asInt' ('-':xs) = either f1 f2 $ helper' xs
    where f2 x = Right $ -1 * x
          f1 x = Left x
asInt' xs = helper' xs

myConcat :: [[a]] -> [a]
myConcat = foldr step []
    where step x acc = x ++ acc

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f = foldr step []
    where step x acc | f x == True = x : acc
                     | otherwise = acc

--fufuuuuuuuu
myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy f = foldr step []
    where step x [] = [[x]]
          step x (y:acc) | f x (head y) == True = (x:y):acc
                         | otherwise = [x]:(y:acc)

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldl' step False
    where step acc x = (f x) || acc

myCycle xs = xs ++ myCycle xs

isPrime x = helper 2 x
    where helper d x | d * d > x = True
                     | x `mod` d == 0 = False
                     | otherwise = helper (d + 1) x

myElem x = foldl (\acc y -> acc || x == y) False

myMaximum :: (Ord a) => [a] -> a
myMaximum = foldr1 (\x acc -> max x acc)

myReverse = foldr (\x acc -> acc ++ [x]) []

myReverse' = foldl (\acc x -> x:acc) []

myProduct = foldr (\x acc -> x * acc) 1

myFilter f = foldr step []
    where step x acc | f x == True = x:acc
                     | otherwise = acc

myFilter' f = foldr (\x acc -> if f x == True then x:acc else acc) []

myHead = foldl1 (\acc x -> acc)

myLast = foldr1 (\x acc -> acc)

myMap f = foldr (\x acc -> (f x):acc) []

data List a = Empty | Cons a (List a)
              deriving (Show)

fib x1 x2 = x1 : (fib x2 $ x1 + x2)

