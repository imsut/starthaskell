-- 1
e1 f p xs = [f x | x <- xs, p x ]
e2 f p xs = map f $ filter p xs

-- 2
all' p xs = and $ map p xs

any' p xs = or $ map p xs

takeWhile' _ [] = []
takeWhile' p (x:xs) = if p x then x : (takeWhile p xs) else []

dropWhile' _ [] = []
dropWhile' p (x:xs) = if p x then dropWhile p xs else x:xs

-- 3
map' f = foldr (\x a -> (f x):a) [] 
filter' p = foldr (\x a -> if p x then x:a else a) []

-- 4
dec2int = foldl (\n d -> n * 10 + d) 0

-- 5
compose = foldl (.) id
--sumsqeven = compose [sum, map (^2), filter even]

-- 6
curry :: ((a, b) -> c) -> a -> b -> c
curry f = \x -> \y -> f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f = \(x, y) -> f x y

-- 7
unfold p h t x | p x		= []
	       | otherwise	= h x : unfold p h t (t x)	

int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8' = unfold null (take 8) (drop 8)
map'' f = unfold null (f . head) tail
iterate' f = unfold (const False) id f




