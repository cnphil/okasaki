-- Exercise 4.2
-- (insertion sort)

sort :: (Ord a) => [a] -> [a]
sort = sort' []
    where
        sort' xs [] = xs
        sort' xs (y:ys) = sort' (insert y xs) ys
        insert x [] = [x]
        insert x (y:ys)
            | x <= y = x:y:ys
            | otherwise = y:(insert x ys)
-- (take k $ sort xs) should take O(kn) time
