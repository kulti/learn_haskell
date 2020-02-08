maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list has not maximum"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Integral a) => a -> b -> [b]
replicate' 0 _ = []
replicate' n y = y : replicate' (n-1) y

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [a] ->[(a, a)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys)
    | x == y = True
    | otherwise = elem' x ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort lh ++ [x] ++ quicksort rh
    where lh = [a | a <- xs, a < x]
          rh = [a | a <- xs, a >= x]
