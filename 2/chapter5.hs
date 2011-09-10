
-- 1
sum100squared = sum [ x * x | x <- [1..100]]

-- 2
repl n x = [ x | _ <- [1..n]]

-- 3
pyths :: Int -> [(Int, Int, Int)]
pyths m = [(x, y, z) | x <- [1..m], y <- [1..m], z <- [1..m], x * x + y * y == z * z]

-- 4
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], (sum $ init $ factors x) == x]

-- 5
-- [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]]
q5 = concat [[(x, y) | x <- [1,2,3]] | y <- [4,5,6]]

-- 6
find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
	where n = length xs - 1

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x $ zip xs [0..n]
	where n = length xs - 1

-- 7
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- 8


