-- ==========
-- Queue
-- ==========
--
-- A queue with O(1) time head and snoc (push),
-- and O(1) amortized time tail.

type Queue a = ([a], [a])

empty = ([], [])

isEmpty :: Queue a -> Bool
isEmpty (f, _) = null f

checkf :: Queue a -> Queue a
checkf ([], r) = (reverse r, [])
checkf q = q

-- snoc appends a simple element at the end of the queue
snoc :: Queue a -> a -> Queue a
snoc (f, r) x = checkf (f, x:r)

head :: Queue a -> a
head ([], _) = error "empty queue"
head (x:f, r) = x

tail :: Queue a -> Queue a
tail (x:f, r) = checkf (f, r)

-- Banker's: one credit for every element at the rear list
-- Physicist's: the potential is the length of the rear list
