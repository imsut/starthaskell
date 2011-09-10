import Prelude(Bool, Int, (+), (-), (*), (&&), (++), (==), (<), (>), Eq, Ord)
import GHC.Bool
import qualified GHC.List(length, take, drop)
import GHC.Real(div)

-- 1
(^) :: Int -> Int -> Int
n ^ 0 = 1
n ^ m = n * (n ^ (m - 1))

-- 2

-- 3
and :: [Bool] -> Bool
and [] = True
and (b:bs) = b && and bs

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ (concat xs)

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : (replicate (n - 1) x)

(!!) :: [a] -> Int -> a
(x:_) !! 0 = x
(x:xs) !! n = xs !! (n - 1)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem a (x:xs) = if a == x then True else elem a xs

-- 4
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x < y then x:(merge xs (y:ys)) else y:(merge (x:xs) ys)

-- 5
msort :: Ord a => [a] -> [a]
msort [] = []
msort (x:[]) = x:[]
msort xs = merge (msort ys) (msort zs)
	where
		halve :: [a] -> ([a], [a])
		halve ws = (GHC.List.take n ws, GHC.List.drop n ws)
			where n = GHC.List.length ws `div` 2

		(ys, zs) = halve xs

-- 6
sum (x:xs) = x + sum xs
sum [] = 0

take 0 xs = xs
take n (x:xs) = take (n - 1) xs

last (x:[]) = x
last (x:xs) = last xs

