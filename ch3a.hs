-- ==========
-- Leftist Heaps
-- ==========
--
-- Exercise 3.1:
-- Proof:
--  Let f(x) be the minimum number of nodes of a leftist heap having a right spine of x.
--  f(x) = 2 * f(x-1) + 1
--  f(1) = 1
--  ...
--  f(x) = 2^x - 1
--  let g = f^(-1)
--  g(n) = log (n+1)
--  Q.E.D.

data Heap a = Empty | Node Int a (Heap a) (Heap a)
              deriving (Show)

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge h Empty = h
merge Empty h = h
merge h1@(Node _ v1 l1 r1) h2@(Node _ v2 l2 r2) =
    if v1 < v2 then makeT v1 l1 (merge r1 h2)
    else makeT v2 l2 (merge r2 h1)
-- this runs in O(log n) because ranks <= O(log n)

-- rank is the right spine length
rank Empty = 0
rank (Node r _ _ _) = r

makeT :: (Ord a) => a -> Heap a -> Heap a -> Heap a
makeT v l r =
    if rl >= rr then Node (rr+1) v l r
    else Node (rl+1) v r l
    where rl = rank l
          rr = rank r

insert :: (Ord a) => a -> Heap a -> Heap a
insert v h = merge (Node 1 v Empty Empty) h

findMin :: Heap a -> a
findMin Empty = error "empty heap"
findMin (Node _ v _ _) = v

deleteMin :: (Ord a) => Heap a -> Heap a
deleteMin Empty = error "empty heap"
deleteMin (Node _ v l r) = merge l r

-- Exercise 3.2

insert2 :: (Ord a) => a -> Heap a -> Heap a
insert2 v Empty = Node 1 v Empty Empty
insert2 x h@(Node _ y l r) =
    if x < y then Node 1 x h Empty
    else makeT y l (insert2 x r)

-- Exercise 3.3

fromList :: (Ord a) => [a] -> Heap a
fromList [] = Empty
fromList ns = fromList' $ map (\v -> Node 0 v Empty Empty) ns
    where fromList' hs = if length hs > 1 then fromList' (oneRound hs) else head hs
          oneRound [] = []
          oneRound [h] = [h]
          oneRound (h1:h2:rest) = (merge h1 h2) : (oneRound rest)
-- time analysis: O(log n) rounds, size of each round: 1 2 4 .. n
--                for each round of size k = 2^m, n/(2^m) * m, where 0 <= m <= log n
--                n/(2^m) * m = n * (m / 2^m)
--                which means O(n) overall time?

-- Exercise 3.4
-- size-based leftist heap, skipped

