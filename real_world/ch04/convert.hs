import Data.Char (digitToInt, isDigit)

type ErrorMessage = String

asInt :: String -> Either ErrorMessage Int
asInt "" = Left "cannot convert empty number"
asInt all@(x:xs)
    | x == '-' = negate <$> asInt_positive xs
    | otherwise = asInt_positive all

asInt_positive :: String -> Either ErrorMessage Int
asInt_positive xs
    | xs == "" = Left "cannot convert empty number"
    | otherwise = foldl (\acc x -> if isDigit x then (+ digitToInt x) <$> (*10) <$> acc else (Left "not a digit")) (Right 0) xs
