factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

max' :: (Ord a) => a -> a -> a
max' x y | x > y     = x
         | otherwise = y

compare' :: (Ord a) => a -> a -> Ordering
compare' x y | x > y     = GT
             | x < y     = LT
             | otherwise = EQ
