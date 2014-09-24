-- ==========
-- Real time queues
-- ==========

data Queue a = Queue { f :: [a], r :: [a], s :: [a] }
    deriving (Show)
-- invariant: |s| = |f| - |r|

empty :: Queue a
empty = Queue [] [] []

rotate :: [a] -> [a] -> [a] -> [a]
rotate [] (y:_) a = y:a
rotate (x:xs) (y:ys) a = x:(rotate xs ys (y:a))

snoc :: Queue a -> a -> Queue a
snoc q x = exec (f q) (x:(r q)) (s q)

hd :: Queue a -> a
hd q = head (f q)

tl :: Queue a -> Queue a
tl q = exec (tail $ f q) (r q) (s q)

exec :: [a] -> [a] -> [a] -> Queue a
exec f r (x:s) = Queue f r s
exec f r _ = Queue f' [] f' where f' = rotate f r []

-- Exercise 7.1
-- If a suspension (a rotate) depends on another one, that one must be one half less in size. O(log n), since intrinsic cost of every suspension is O(1)

-- Exercise 7.2
-- s is a suffix of f, so might be slightly faster? But since the size of r is O(n), there won't be any difference in Big-O notation.
