-- ==========
-- Pairing Heaps
-- ==========
-- Proved: insert, merge, deleteMin in O(log n) amortized time
-- Conjectured: insert, merge in O(1) amortized time

data Heap a = Empty
            | Node a [Heap a]

findMin :: Heap a -> a
findMin (Node x _) = x

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge Empty t = t
merge t Empty = t
merge t1@(Node x1 ts1) t2@(Node x2 ts2) =
    if x1 <= x2 then
        Node x1 (t2:ts1)
    else
        Node x2 (t1:ts2)

insert :: (Ord a) => a -> Heap a -> Heap a
insert x t = merge (Node x []) t

mergePairs [] = E
mergePairs [t] = t
mergePairs (t1:t2:ts) = merge (merge t1 t2) (mergePairs ts)

deleteMin (Node x ts) = mergePairs ts

-- Exercise 5.8
-- skipped
--
-- Exercise 5.9
-- skipped
