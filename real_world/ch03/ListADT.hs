data List a = Cons a (List a)
            | Nil
              deriving (Eq)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Show a => Show (List a) where
  show xs = "["++ concat xs ++"]"
    where
      concat :: Show a => List a -> String
      concat Nil = ""
      concat (Cons x Nil) = show x
      concat (Cons x xs) = show x ++ "," ++ concat xs

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : fromList xs

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)

listLength :: Integral b => List a -> b
listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listSum :: Num a => List a -> a
listSum Nil = 0
listSum (Cons x xs) = x + (listSum xs)

listMean :: (Fractional a) => List a -> a
listMean Nil = 0
listMean xs = (listSum xs)/fromIntegral(listLength xs)

listReverse :: List a -> List a
listReverse Nil = Nil
listReverse (Cons x xs) = listAppend (listReverse xs) (Cons x Nil)

listAppend :: List a -> List a -> List a
listAppend l Nil = l
listAppend Nil (Cons y ys) = Cons y ys
listAppend (Cons x xs) ys = Cons x $ listAppend xs ys

palindromList :: List a -> List a
palindromList xs = listAppend xs $ listReverse xs

listIsPalindrom :: Eq a => List a -> Bool
listIsPalindrom xs = xs == listReverse xs

filterList :: (a -> Bool) -> List a -> List a
filterList f Nil = Nil
filterList f (Cons x xs) = if f x then Cons x filteredTail else filteredTail
  where filteredTail = filterList f xs

sortListByLen :: List (List a) -> List (List a)
sortListByLen Nil = Nil
sortListByLen (Cons x xs) = listAppend (sortListByLen xl) (Cons x (sortListByLen xr))
  where lx = listLength x
        xl = filterList (\x1 -> listLength(x1) < lx) xs
        xr = filterList (\x1 -> listLength(x1) > lx) xs

concatList :: (Show a) => List a -> String
concatList Nil = ""
concatList (Cons x Nil) = show x
concatList (Cons x xs) = show x ++ "," ++ concatList xs
