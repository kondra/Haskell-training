-- Functor instance for []
fm _ [] = []
fm g (h:t) = (g h) : (g h) : (fm g t)

-- Functor instance for Maybe
mm _ Nothing = Nothing
mm g (Just a) = Just (g a)

-- bind & unit
f,g :: Int -> Int
f = (+) 1
g = (+) 2

f',g' :: Int -> (Int, String)
f' x = (f x, "f was called")
g' x = (g x, "g was called")

bind :: (Int->(Int, String))->(Int, String)->(Int, String)
bind f (n, s) = (z, x ++ s) where (z, x) = f n

unit :: Int -> (Int, String)
unit x = (x, "unit")

-- Monad instance for Maybe
data MyMaybe a = MyJust a
               | MyNothing
               deriving (Show)

instance Monad MyMaybe where
    return x = MyJust x
    (>>=) MyNothing _ = MyNothing
    (>>=) (MyJust a) f = f a
