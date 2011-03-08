import Control.Applicative
import Data.Monoid
import qualified Data.Foldable as F

data List a = Empty
            | Cons a (List a)
            deriving (Show)

instance Functor List where
    fmap _ Empty = Empty
    fmap f (Cons x xs) = Cons (f x) $ fmap f xs

join _ Empty = Empty
join Empty (Cons y ys) = Cons y $ join Empty ys
join (Cons x xs) ys = Cons x $ join xs ys

instance Monoid (List a) where
    mempty = Empty
    mappend = join

instance F.Foldable List where
    foldMap f Empty = mempty
    foldMap f (Cons x xs) = (f x) `mappend` (F.foldMap f xs)


myReverse :: List a -> List a
myReverse = F.foldr (\x acc -> acc `join` (Cons x Empty)) Empty

