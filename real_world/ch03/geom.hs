import Data.Maybe
import Data.List (sortBy)

data RotateDirection = ClockWise | AntiClockWise | Straight deriving(Show, Eq)

data Point a = Point {x :: a, y :: a} deriving(Eq)

instance (Show a) => Show (Point a) where
    show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"

type Vector = Point

vectorFromPoints :: (Num a) => Point a -> Point a -> Vector a
vectorFromPoints p1 p2 = Point (x p2 - x p1) (y p2 - y p1)

vectorLength :: (Floating a) => Vector a -> a
vectorLength v = sqrt((x v) ^ 2 + (y v) ^ 2)

vectorProduct :: (Num a) => Vector a -> Vector a -> a
vectorProduct v1 v2 = (x v1) * (y v2) - (y v1) * (x v2)

scalarProduct :: (Num a) => Vector a -> Vector a -> a
scalarProduct v1 v2 = (x v1) * (x v2) + (y v1) * (y v2)

checkRotateDirection :: (Num a, Ord a) => Point a -> Point a -> Point a -> RotateDirection
checkRotateDirection p0 p1 p2
    | r < 0 = AntiClockWise
    | r > 0 = ClockWise
    | otherwise = Straight
        where v1 = vectorFromPoints p0 p1
              v2 = vectorFromPoints p0 p2
              r = vectorProduct v1 v2

succesiveDirections :: (Num a, Ord a) => [Point a] -> Maybe [RotateDirection]
succesiveDirections [] = Nothing
succesiveDirections [_] = Nothing
succesiveDirections [_, _] = Nothing
succesiveDirections [a, b ,c] = Just [checkRotateDirection a b c]
succesiveDirections (a : b : c : xs) = Just (checkRotateDirection a b c : fromJust (succesiveDirections (b : c : xs)))

graham :: (Floating a, Ord a) => [Point a] -> [Point a]
graham xs
    | length xs < 3 = xs
    | otherwise = graham' xs

graham' :: (Floating a, Ord a) => [Point a] -> [Point a]
graham' xs = s
    where
        lowest = foldr1 (\v acc -> if y v < y acc then v else if x v < x acc then v else acc) xs
        pp = Point (x lowest + 1) (y lowest)
        a = angle lowest pp
        ss = sortBy (\v1 v2 -> if a v1 > a v2 then LT
                              else if a v1 < a v2 then GT
                              else if x v1 + y v1 < x v2 + y v2 then LT
                              else GT) xs
        (x1 : x2 : x3) = ss
        s = foldl (\(x1 : x2 : xs) x -> if (checkRotateDirection x1 x2 x) == AntiClockWise then x : x1 : x2 : xs else x : x2 : xs) [x2, x1] x3

angle :: (Floating a, Eq a) => Point a -> Point a -> Point a -> a
angle p0 p1 p2
    | l1 == 0 = 1
    | l2 == 0 = 1
    | otherwise = scalar/l1/l2
    where
        v1 = vectorFromPoints p0 p1
        v2 = vectorFromPoints p0 p2
        scalar = scalarProduct v1 v2
        l1 = vectorLength v1
        l2 = vectorLength v2
