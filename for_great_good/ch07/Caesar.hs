module Caesar(
    encode,
    decode
) where

import Data.Char
import qualified Data.Set as Set

lowerLatin = Set.fromList ['a'..'z']
upperLatin = Set.fromList ['A'..'Z']

encode :: Int -> String -> String
encode shift msg = map (encodeChar shift) msg

encodeChar shift x
    | x `elem` lowerLatin = chr (ord 'a' + (ord x + shift - ord 'a') `mod` 26)
    | x `elem` upperLatin = chr (ord 'A' + (ord x + shift - ord 'A') `mod` 26)
    | otherwise = x

decode :: Int -> String -> String
decode shift msg = map (decodeChar shift) msg

decodeChar shift x
    | x `elem` lowerLatin = chr (ord 'a' + (ord x - shift - ord 'a') `mod` 26)
    | x `elem` upperLatin = chr (ord 'A' + (ord x - shift - ord 'A') `mod` 26)
    | otherwise = x
