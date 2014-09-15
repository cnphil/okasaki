-- Exercise 5.1
-- ==========
-- Deque
-- ==========
-- each operation takes O(1) amortized time.
-- the potential function is Phi(f, r) = abs((len f)-(len r))

type Deque a = ([a], [a])

empty = ([], [])

isEmpty :: Deque a -> Bool
isEmpty (f, _) = null f

head :: Deque a -> a
head ([], _) = error "empty deque"
head (x:_, _) = x
-- obviously O(1)

tail :: Deque a -> Deque a
tail ([], _) = error "empty deque"
tail (x:f, r) = check (f, r)
-- if none of lists is empty, tail is O(1);
-- but when f becomes empty:
--      before
--          len f = 1
--          len r = m
--          potential = m - 1
--      after
--          len f = (m+1) / 2
--          len r = m / 2
--          potential = 1 (when m is odd)
--                      0 (when m is even)
-- actual cost:
--     +1 for x
--     +m for creating the lists
-- amortized costs:
--      actual cost + potential's difference
--      = (m + 1) - (0 - (m - 1))
--      = (m + 1) - (1 - (m - 1))   -- when m is odd
--      = O(1)

cons :: a -> Deque a -> Deque a
cons x (f, r) = check (x:f, r)
-- obviously O(1)

snoc :: Deque a -> a -> Deque a
snoc (f, r) x = check (f, x:r)
-- obviously O(1)

last :: Deque a -> a
last ([], []) = error "empty deque"
last ([x], []) = x
last (_, x:r) = x
-- obviously O(1)

init :: Deque a -> Deque a
init ([], []) = error "empty deque"
init ([_], []) = ([], [])
init (f, x:r) = check (f, r)
-- similar to tail, O(1)

check :: Deque a -> Deque a
check (f, r) =
        if null f then
            (reverse $ drop (botHalf r) r, take (botHalf r) r)
        else if null r then
            (take (topHalf f) f, reverse $ drop (topHalf f) f)
        else
            (f, r)
    where topHalf l = (length l + 1) `div` 2
          botHalf l = (length l) `div` 2
-- assume this takes (length l) steps
-- (that drop and take are actually implemented as a single splitAt)

-- Exercise 5.2
-- one credit for each tree in the heap
--
-- Exercise 5.3
-- say we are merging two heaps of m1 trees and m2 trees,
-- merging them should take m1 + m2 + k + 1 steps where k is the number of
-- merges, the potential difference would be (m1+m2-k)-(m1+m2) = -k
-- amortized cost = (m1+m2+1) = O(log n)
-- deleteMin is just a call to merge
