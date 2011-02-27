solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl (helper) [] . words
    where helper (x:y:ys) "*" = (x * y) : ys
          helper (x:y:ys) "+" = (x + y) : ys
          helper (x:y:ys) "-" = (y - x) : ys
          helper xs str = read str : xs

buildRPN :: String -> String
buildRPN = unwords . reverse . final . buildRPN'
    where final (xs, []) = xs
          final (xs, ys) = xs ++ (reverse ys)

buildRPN' :: String -> ([String], [String])
buildRPN' = foldl (helper) ([], []) . words
    where helper (xs, ys) "*" = (xs, "*" : ys)
          helper (xs, ys) "+" = (xs, "+" : ys)
          helper (xs, ys) "-" = (xs, "-" : ys)
          helper (xs, ys) "(" = (xs, "(" : ys)
          helper (xs, ys) ")" = (xs', ys')
              where xs' = (takeWhile ((/=) "(") ys) ++ xs
                    ys' = let ys'' = dropWhile ((/=) "(") ys in
                              if null ys''
                                 then []
                                 else reverse $ tail ys''
          helper (xs, y:ys) num | y /= "(" = (y : num : xs, ys)
                                | otherwise = (num : xs, y:ys)
          helper (xs, ys) num = (num : xs, ys)
