safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just $ init xs

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs = ls : (splitWith f rs)
    where (ls, rrs) = span f xs
          rs = if null rrs then [] else drop 1 rrs

concat' :: [[a]] -> [a]
concat' [] = []
concat' xs = foldr1 (++) xs

concat'' :: [[a]] -> [a]
concat'' (x:xs) = foldr (:) (concat'' xs) x
concat'' _ = []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) = if f x then x : takeWhile' f xs else []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' f = foldr (\x acc -> if f x then x:acc else []) []

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f = foldr (\x acc -> if null acc then [[x]] else g f x acc) []
    where g f x (y:ys) = if x `f` head y then (x:y) : ys else [x] : y : ys

any' :: (a -> Bool) -> [a] -> Bool
any' f = foldr (\x acc -> if f x then True else acc) False

cycle' :: [a] -> [a]
cycle' [] = error "empty list"
cycle' xs = foldr (:) (cycle' xs) xs

words' :: String -> [String]
words' = filter ("" /=) . foldr (\x acc -> if null acc then [[x]] else g x acc) []
    where g x (y:ys) = if x == ' ' then [] : y : ys else (x:y) : ys

unlines' :: [String] -> String
unlines' = foldr (\x acc -> x ++ "\n" ++ acc) ""
