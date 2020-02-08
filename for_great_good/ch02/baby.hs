doubleUs x y = doubleMe x + doubleMe y

doubleMe x = x + x

doubleSmallNumber x = if x > 100 then x else x * 2

length' xs = sum [ 1 | _ <- xs ]

m xxs = [ [ x | x <- xs, even x ] | xs <- xxs ]
