zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- doesn't work with inifinite list
elem' :: (Eq a) => a -> [a] -> Bool
elem' x xs = foldl (||) False (map (==x) xs)

-- doesn't work with inifinite list too
elem'' :: (Eq a) => a -> [a] -> Bool
elem'' x xs = foldl (\acc y -> if x == y then True else acc) False xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> (f x) : acc) [] xs

reverse' :: [a] -> [a]
reverse' xs = foldl (\acc x -> x : acc) [] xs

maximum' :: (Ord a) => [a] -> a
maximum' xs = foldl1 (\acc x -> if x > acc then x else acc) xs

product' :: (Num a) => [a] -> a
product' = foldl1 (\acc x -> x * acc)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x acc -> if f x then x:acc else acc) []
