-- ============
-- Queues
-- ============
-- Amortized time of O(1) for each operation.
-- Used persistently and lazyly.
-- (reverses the latter list when it outgrows the former one)

data Queue a = Queue Int [a] Int [a]

head :: Queue a -> a
head (Queue _ (x:_) _ _) = x

snoc :: Queue a -> a -> Queue a
snoc (Queue lf f lr r) x = check $ Queue lf f (lr+1) (x:r)

tail :: Queue a -> Queue a
tail (Queue lf (f:fs) lr rs) = check $ Queue (lf-1) fs lr rs

check :: Queue a -> Queue a
check orig@(Queue lf f lr r) =
                          if lr > lf then
                            Queue (lf+lr) (f ++ (reverse r)) 0 []
                          else orig

size :: Queue a -> Int
size (Queue lf _ lr _) = lf + lr

empty :: Queue a
empty = Queue 0 [] 0 []

isEmpty q = size q == 0

-- Exercise 6.1
-- skipped
--
-- Exercise 6.2
-- this means that we reverse the latter list when is outgrows the twice of the former one.
-- we'll have snoc and tail to both discharge 2 debits to solve this problem. O(1) amortized.
-- comparison: 100 snocs followed by 100 tails
-- the original version shall create 7 reverses (for 1, 3, 7, 15, 31, 63, 37)
-- the exercise version shall create 5 reverses (for 1, 4, 13, 40, 20)
-- it seems the exercise version is slightly better?
-- 
-- Exercise 6.3-6.4
-- they are O(log n) in the worst case.
--
-- Exercise 6.5
-- skipped
--
-- Exercise 6.6
-- skipped (but worth thinking)
--
-- Exercise 6.7
-- skipped (this section is damn hard...)
