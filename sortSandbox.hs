
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (p : xs) = quicksort [x | x <- xs, x < p] ++ [p] ++  quicksort [x | x <- xs, x > p]


mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort left) (mergesort right)
               where (left, right) = split xs


split :: [a] -> ([a], [a])
split (x:y:zs) = (x:xs, y:ys)
               where (xs, ys) = split zs
split xs = (xs, [])

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y
                      then x : merge xs (y:ys)
                      else y : merge (x:xs) ys