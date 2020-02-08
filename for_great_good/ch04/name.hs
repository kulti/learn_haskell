initials :: String -> String -> String
initials (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."
