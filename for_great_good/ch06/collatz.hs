collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz x
    | even x = x : (collatz (div x 2))
    | odd x  = x : (collatz (x*3+1))
