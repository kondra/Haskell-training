safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast [] = Nothing
safeLast (x:xs) | null xs = Just x
                | otherwise = safeLast xs

safeInit [] = []
safeInit (x:xs) | null xs = []
                | otherwise = x : (safeInit xs)
