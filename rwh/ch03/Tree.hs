data Tree a = Node {
            val        :: a,
			leftChild  :: (Tree a),
			rightChild :: (Tree a)
			}
            | Empty
			  deriving (Show)
