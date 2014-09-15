-- ==========
-- Red Black Trees
-- ==========

data Color = Red | Black
data Tree a = Node { color :: Color, value :: a, subl :: (Tree a), subr :: (Tree a) }
            | Empty
-- empty nodes considered to be black
-- (*) no red node has a red child
-- (*) every path from root to empty node contains the same number
-- of black nodes

-- Exercise 3.8
-- skipped

member :: (Ord a) => a -> Tree a -> Bool
member _ Empty = False
member x t =
    if x < value t then member x (subl t)
    else if x > value t then member x (subr t)
    else True

insert :: (Ord a) => a -> Tree a -> Tree a
insert x s = Node Black v' l' r'
    where ins Empty = Node Red x Empty Empty
          ins t =
                if x < value t then
                    balance (color t) (value t) (ins $ subl t) (subr t)
                else if x > value t then
                    balance (color t) (value t) (subl t) (ins $ subr t)
                else t
          Node _ v' l' r' = ins s

balance :: Color -> a -> Tree a -> Tree a -> Tree a
balance Black z (Node Red x a (Node Red y b c)) d = Node Red y (Node Black x a b) (Node Black z c d)
balance Black z (Node Red y (Node Red x a b) c) d = Node Red y (Node Black x a b) (Node Black z c d)
balance Black x a (Node Red y b (Node Red z c d)) = Node Red y (Node Black x a b) (Node Black z c d)
balance Black x a (Node Red z (Node Red y b c) d) = Node Red y (Node Black x a b) (Node Black z c d)
balance c v l r = Node c v l r

-- Exercise 3.9
-- Refer to Constructing Red-Black Trees by Ralf Hinze

data Digit a = One a (Tree a)
             | Two a (Tree a) a (Tree a)

-- numbers are fed in decreasing order
incr :: Digit a -> [Digit a] -> [Digit a]
incr (One a t) [] = [One a t]
incr (One a1 t1) ((One a2 t2) : ds) = Two a1 t1 a2 t2 : ds
incr (One a1 t1) ((Two a2 t2 a3 t3) : ds) = One a1 t1 : (incr (One a2 (Node Black a3 t2 t3)) ds)

fromOrdList :: (Ord a) => [a] -> Tree a
fromOrdList = linkAll . (foldr add [])
add a ds = incr (One a Empty) ds
linkAll = foldl link Empty
link l (One a t) = Node Black a l t
link l (Two a1 t1 a2 t2) = Node Black a2 (Node Red a1 l t1) t2

-- Exercise 3.10
-- skipped
