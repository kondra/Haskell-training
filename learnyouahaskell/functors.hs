import Control.Applicative

data MyMaybe a = MyJust a
               | MyNothing
               deriving (Show)

instance Functor MyMaybe where
    fmap _ MyNothing = MyNothing
    fmap f (MyJust a) = MyJust $ f a

instance Applicative MyMaybe where
    pure = MyJust

    (MyJust f) <*> x = fmap f x
    MyNothing <*> _ = MyNothing

instance Monad MyMaybe where
    return = MyJust
    MyJust x >>= f = f x
    MyNothing >>= _ = MyNothing
    fail _ = MyNothing

data List a = Empty
            | Cons a (List a)
            deriving (Show)

instance Functor List where
    fmap _ Empty = Empty
    fmap f (Cons x xs) = Cons (f x) $ fmap f xs

{-
concatList = 

instance Monad List where
    return = Cons
    xs >>= f = concat $ map f xs
-}

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' = foldr (liftA2 (:)) (pure [])
