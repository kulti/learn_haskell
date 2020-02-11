lastButOne :: [a] -> a
lastButOne []     = error "empty list has not last but one element"
lastButOne [_]    = error "one element list has not last but one element"
lastButOne [x, _] = x
lastButOne (_:xs) = lastButOne xs
