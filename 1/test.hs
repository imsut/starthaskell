

hd (a:as) = a

n = a `div` length xs
    where
	a = 10
 	xs = [1..5]

last1 x = head $ reverse x

last2 (a:[]) = a
last2 (a:as) = last as

init1 x = take (length x - 1) x

init2 (x:[]) = []
init2 (x:xs) = x:(init2 xs)

takeLast n xs = reverse $ take n $ reverse xs

takeLast' n xs | n < length xs = takeLast' n $ tail xs
	       | otherwise = xs

halve xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
      	 where n = length xs


safeTail1 xs = if length xs > 0 then tail xs else []
safeTail2 xs | length xs > 0 = tail xs
	     | otherwise = []

safeTail3 [] = []
safeTail3 x  = tail x


mult = \x -> \y -> \z -> x * y * z

fib :: Integral n => n -> n
fib n = let root5 = sqrt 5.0
      	    nf = fromIntegral n
      	in
		round (((1.0 + root5) ** nf - (1.0 - root5) ** nf) / (2.0 ** nf * root5))
