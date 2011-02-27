lastButOne (x:y:xs) = if null xs
                      then x
					  else lastButOne (y:xs)
