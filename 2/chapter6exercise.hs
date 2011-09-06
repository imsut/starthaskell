module Chapter6 where

import Prelude hiding (take)

-- 6.2
{-
length [1,2,3]
-> 1 + length [2, 3]
-> 1 + (1 + length [3])
-> 1 + (1 + (1 + length []))
-> 1 + (1 + (1 + 0))
-> 3

drop 3 [1, 2, 3, 4, 5]
-> drop 2 [2, 3, 4, 5]
-> drop 1 [3, 4, 5]
-> drop 0 [4, 5]
-> [4, 5]

init [1, 2, 3]
-> 1 : init [2, 3]
-> 1 : (2 : init [3])
-> 1 : (2 : [])
-> [1, 2]
-}

-- 6.6
take          :: Int -> [a] -> [a]
take 0 _      = []
take _ []     = []
take n (x:xs) = x : take (n - 1) xs


last'        :: [a] -> a
last' (x:[])     = x
last' (_:xs)    = last' xs
last' []    = error "empty"

{-
foldl'            :: (a -> b -> a) -> a -> [b] -> a
foldl' _ iv []        = iv
foldl' f iv [x]        = f iv x
foldl' f iv (x:xs)    = foldr (\x n -> f n x) iv xs
-}

-- ex.9

sp      :: Int -> [a] -> ([a], [a])
sp _ [] = ([], [])
sp n (x:xs)
    | n > 0     = (x:ys, zs)
    | otherwise = ([], x:xs)
  where
    (ys, zs) = sp (n-1) xs

test9a :: [([Int], [Int])]
test9a = [
    sp 2 [1..4],
    sp 0 [1..4],
    sp 4 [1..4],
    sp 6 [1..4],
    sp (-1) [1..4],
    sp 2 []
    ]

sp' :: Int -> [a] -> ([a], [a])
sp' = sp'' ([],[])
  where
    sp'' (l1,l2) _ []    = (reverse l1, reverse l2)
    sp'' (l1,l2) m (l:ls)
      | m > 0        = sp'' (l:l1,l2) (m-1) ls
      | otherwise    = sp'' (l1,l:l2) (m-1) ls

test9b :: [([Int], [Int])]
test9b = [
    sp' 2 [1..4],
    sp' 0 [1..4],
    sp' 4 [1..4],
    sp' 6 [1..4],
    sp' (-1) [1..4],
    sp' 2 []
    ]

{-
main = do (xs, ys) = sp 2 [1..5]
          printf "%d\t%d\n" (length xs) (length ys)
-}

