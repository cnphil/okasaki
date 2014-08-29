import Prelude hiding (lookup)
-- ==========
-- Lists
-- ==========
--
-- Exercise 2.1
-- write a function of [a] -> [[a]] that returns all the suffixes of that list in O(n) time and space.

suffixes [] = [[]]
suffixes l@(_:xs) = l : (suffixes xs)

-- reasoning:
-- note that every l of each recursive calls will be reused,
-- none of the original list's items will be copied.


-- ==========
-- Binary Search Trees
-- ==========

data Tree a = Empty | Node (Tree a) a (Tree a)
     deriving (Show, Eq)

testTree = insert 1 $ insert 5 $ insert 7 $ insert 2 $ insert 4 $ insert 6 $ insert 3 Empty

member :: (Ord a) => a -> Tree a -> Bool
member _ Empty = False
member x t@(Node l v r) =
            if x < v then
                member x l
            else if x > v then
                member x r
            else True

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x t@(Node l v r) =
            if x < v then
                Node (insert x l) v r
            else if x > v then
                Node l v (insert x r)
            else t

-- Exercise 2.2

member2 :: (Ord a) => a -> Tree a -> Bool
member2 _ Empty = False
member2 x t@(Node l v r) = member' t v
    where member' Empty b = (x == b)
          member' (Node ll vv rr) b =
                    if x < vv then
                        member' ll b
                    else
                        member' rr vv

-- Exercise 2.3 and 2.4
-- i can't think of a better solution...

-- Exercise 2.5

complete :: a -> Int -> Tree a
complete x 0 = Empty
complete x d = Node st x st
    where st = complete x (d-1)
-- this should run in O(d) time

balanced :: a -> Int -> Tree a
balanced x 0 = Empty
balanced x sz
    | even (sz-1)  =  let subtree = balanced x ((sz-1) `div` 2) in Node subtree x subtree
    | otherwise  =  Node stl x str
        where (stl, str) = create2 ((sz-1) `div` 2)
              create2 m = (balanced x m, balanced x (m+1))
-- this should run in O(log n) time

-- Exercise 2.6

data MapNode k v = MapNode { key :: k, value :: v }

instance (Ord k) => Ord (MapNode k v) where
    compare a b = (compare `on` key) a b

instance (Eq k) => Eq (MapNode k v) where
    (==) a b = ((==) `on` key) a b

on f g = \a b -> f (g a) (g b)

type FiniteMap k v = Tree (MapNode k v)

empty :: FiniteMap k v 
empty = Empty

bind :: (Ord k) => k -> v -> FiniteMap k v -> FiniteMap k v
bind kk vv t = if member newNode t then error "duplicate key"
               else insert newNode t
               where newNode = MapNode kk vv

lookup :: (Ord k) => k -> FiniteMap k v -> v
lookup _ Empty = error "not found"
lookup kk t@(Node subl (MapNode rk rv) subr) =
            if kk == rk then rv
            else if kk < rk then lookup kk subl
            else lookup kk subr
