data Direction = DLeft
               | DRight
			   | DStraight
			     deriving (Eq, Show)

data Point = Point {
             x :: Double,
			 y :: Double
			 } deriving (Show)

calcDir a b c | vectProduct (vect b a) (vect b c) < 0 = DLeft
              | vectProduct (vect b a) (vect b c) > 0 = DRight
              | otherwise = DStraight
              where vect a b = Point (x b - x a) (y b - y a)
                    vectProduct a b = (x a) * (y b) - (x b) * (y a)

listDir [] = []
listDir (x:xs) | length xs < 2 = []
               | otherwise = (calcDir x (head xs) (head (tail xs))) : (listDir (xs))
