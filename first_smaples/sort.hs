q [] = []
q (x:t) = q [y | y<-t, y<x] ++ [x] ++ q [y | y<-t, y>=x]
