data Lambda = Var String  	-- Variable
	    | App Lambda Lambda -- Application
	    | Lam String Lambda -- Abstraction

{-- impelent instance Show --}
instance Show Lambda where
    show (Var x) = x
    show (Lam x l) = "\\" ++ x ++ ".(" ++ show l ++ ")"
    show (App l1 l2) = "(" ++ show l1 ++ ")(" ++ show l2 ++ ")"

{-- Свободна ли переменная в терме? --}
free :: [Char] -> Lambda -> Bool
free x (Var y) | x == y = True
               | otherwise = False
free x (Lam s l) = x /= s && (free x l)
free x (App l1 l2) = (free x l1) || (free x l2)

{-- substitute t x r -- подстановка r в t вместо формального параметра x --}
substitute :: Lambda -> String -> Lambda -> Lambda
substitute (Var y) x r | x == y = r
                       | otherwise = Var y
substitute (Lam y l) x r | y == x = substitute l x r
                         | free y r = substitute (substitute (Lam z l) y (Var z)) x r
                         | otherwise = Lam y (substitute l x r)
                         where z = "z"
substitute (App l1 l2) x r = App (substitute l1 x r) (substitute l2 x r)
									
{-- Бета-редукция ( подстановка в абстракцию ) --}
betaRed :: Lambda -> Lambda -> Lambda
betaRed (Lam x l1) l2 = substitute l1 x l2
