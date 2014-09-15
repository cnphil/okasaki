-- ==========
-- Splay Trees
-- ==========
--
-- Exercise 5.4
-- skipped (see blow)
--
-- Exercise 5.5
-- skipped
--
-- Exercise 5.6
-- Proof: Similar to insert, the additional cost of deleteMin over that of
-- partition is one actual step plus the change in potential between the two
-- subtrees (one of which is empty) returned by partition and the final tree.
-- O(log n)

-- the code below defines a heap implemented by a splay tree

data Heap a = Empty
            | Node (Heap a) a (Heap a)

isEmpty Empty = True
isEmpty _ = False

-- (partition pivot heap) returns the two subtrees divided by pivot
partition :: (Ord a) => a -> Heap a -> (Heap a, Heap a)
partition _ Empty = (Empty, Empty)
partition pivot t@(Node a x b)
    | x <= pivot = case b of
                    Empty -> (t, Empty)
                    (Node b1 y b2) -> if y <= pivot then
                                        let (small, big) = partition pivot b2
                                        in (Node (Node a x b1) y small, big)
                                      else
                                        let (small, big) = partition pivot b1
                                        in (Node a x small, Node big y b2)
    | otherwise = case a of
                    Empty -> (Empty, t)
                    (Node a1 y a2) -> if y <= pivot then
                                        let (small, big) = partition pivot a2
                                        in (Node a1 y small, Node big x b)
                                      else
                                        let (small, big) = partition pivot a1
                                        in (small, Node big y (Node a2 x b))
                                        
insert :: (Ord a) => a -> Heap a -> Heap a
insert x t = Node a x b
    where (a, b) = partition x t

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge Empty t = t
merge (Node a x b) t = Node (merge ta a) x (merge tb b)
    where (ta, tb) = partition x t

findMin :: Heap a -> a
findMin Empty = error "empty"
findMin (Node Empty x _) = x
findMin (Node a x b) = findMin a

deleteMin :: (Ord a) => Heap a -> Heap a
deleteMin Empty = error "empty"
deleteMin (Node Empty _ b) = b
deleteMin (Node a x b) = Node (deleteMin a) x b

-- Exercise 5.7
sort :: (Ord a) => [a] -> [a]
sort = traverse . fromList
    where fromList = foldr insert Empty
          traverse = reverse . (search [])
          search xs Empty = xs
          search xs (Node a x b) = search (x:search xs a) b
-- for inputs that are already sorted
-- the traversal is obviously O(n)
-- and the proof of fromList is also trivial
-- given that insertion always reconstructs the tree for zig-zig or zag-zag
-- and the depth of new element < 4
