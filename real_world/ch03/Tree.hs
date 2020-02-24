-- data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
--               deriving (Show)

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

treeHight :: Tree a -> Int
treeHight Empty = 0
treeHight (Node x xls xrs) = 1 + max hl hr
    where hl = treeHight xls
          hr = treeHight xrs
