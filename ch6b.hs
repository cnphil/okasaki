-- ==========
-- Lazy Pairing Heaps
-- ==========
--
data Heap a  = Empty
             | Node a (Heap a) (Heap a)

insert :: (Ord a) => a -> Heap a -> Heap a
insert x h = merge (Node x Empty Empty) h

findMin :: Heap a -> a
findMin (Node x _ _) = x

deleteMin :: (Ord a) => Heap a -> Heap a
deleteMin (Node x a b) = merge a b

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge a Empty = a
merge Empty b = b
merge a@(Node x _ _) b@(Node y _ _) =
    if x < y then link a b else link b a

link :: (Ord a) => Heap a -> Heap a -> Heap a
link (Node x Empty m) b = Node x b m
link (Node x a m) b = Node x Empty (merge (merge b a) m)


