import Data.Char

-- 3 
pyths   :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1 .. n],
                       y <- [x .. n], 
                       z <- [y .. n], 
                       x^2 + y^2 == z^2]

pyths'  :: [(Int, Int, Int)]
pyths'  = [(x, y, z) | z <- [1 ..],
                       y <- [1 .. z-1],
                       x <- [1 .. y-1],
                       x^2 + y^2 == z^2]

-- ex.2
vowelPositions      :: String -> [Int]
vowelPositions xs   = [n | (n, c) <- zip [1..] xs, toLower c `elem` "aiueo"]

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
-}

