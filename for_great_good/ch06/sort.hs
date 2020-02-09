quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort lh ++ [x] ++ quicksort rh
    where lh = filter (<x) xs
          rh = filter (>=x) xs
