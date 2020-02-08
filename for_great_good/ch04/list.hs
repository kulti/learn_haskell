head' :: [a] -> a
head' []      = error "empty list has not head"
head' (x : _) = x

tail' :: [a] -> a
tail' []       = error "empty list has not tail"
tail' [x     ] = x
tail' (_ : xs) = tail' xs

length' :: [a] -> Integer
length' []       = 0
length' (_ : xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' []       = 0
sum' (x : xs) = x + sum' xs

drop' :: (Integral a) => a -> [b] -> [b]
drop' _ [] = []
drop' 0 xs = xs
drop' n (x:xs) = drop' (n-1) xs
