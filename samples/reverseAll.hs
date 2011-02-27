--atom :: ListStr (a) -> Bool
--atom a = True
--atom _ = False

append [] l = l
append (h:t) l = h : append t l

--reverseAll :: ListStr (a) -> ListStr (a)
--reverseAll l     = l
reverseAll []    = []
reverseAll (h:t) = (reverseAll t) ++ (reverseAll h)

--atom l = True
--atom _ = False

--reverseAll l | atom l == True = l 
--reverseAll [] = []
--reverseAll (h:t) = (reverseAll h) ++ (reverseAll t)

main = do
	print 1
--	print (reverseAll [[1,2], [1,2,3], [1,[2,3,4,5]]])
