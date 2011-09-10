
-- Exercise 1 ------------------------------------------------------------
magComp :: Float -> Float -> Float
magComp a b = (10 ** (a - b)) ** 1.5


-- Exercise 2 ------------------------------------------------------------
approxEq :: Float -> Float -> Float -> Bool
approxEq delta v1 v2 = abs(v1 - v2) < abs(delta)

approxEq' = approxEq 0.0001
approxZero = approxEq' 0


-- Exercise 3 ------------------------------------------------------------
compLevel :: (Int, Int) -> Float -> Int -> Float -> Float -> Float
compLevel (w, h) fps len audio fileSize = (imageSizeB + audioSizeB) / fileSizeB
	  where
		imageSizeB = fromIntegral ((24 `div` 8) * w * h) * fps * fromIntegral len
		audioSizeB = 1000 * audio * fromIntegral len
		fileSizeB = 1024 * 1024 * fileSize

test3 = [
      compLevel (720, 480) (30000 / 1001) (120 * 60) 128 700,
      compLevel (352, 288) 25 (120 * 60) 128 700
      ]


-- Exercise 4 ------------------------------------------------------------
revmid :: [a] -> [a]
revmid x | length x < 3 = x
       	 | otherwise = (head x :) $ revButLast $ tail x
	 where
		revButLast' (e1:e2:es) r = revButLast' (e2:es) (e1:r)
		revButLast' last r = r ++ last
		revButLast xs = revButLast' xs []

-- probably faster ... ?
revmid' :: [a] -> [a]
revmid' as@(a1:a2:a3:rest) = (a1:) $ revButLast (a2:a3:rest)
	where
		revButLast' (e1:e2:es) r = revButLast' (e2:es) (e1:r)
		revButLast' last r = r ++ last
		revButLast xs = revButLast' xs []
revmid' as = as


-- Exercise 5 ------------------------------------------------------------
isValidTP :: (Float, Float, Float)  -- vertex1 (X,Y,Z)
	  -> (Float, Float, Float)  -- vertex2 (X,Y,Z)
          -> (Float, Float, Float)  -- vertex3 (X,Y,Z)
          -> (Float, Float, Float)  -- vertex4 (X,Y,Z)
          -> (Float, Float, Float)  -- vertex5 (X,Y,Z)
          -> (Float, Float, Float)  -- vertex6 (X,Y,Z)
          -> Bool                   -- True if order is correct

isValidTP v1 v2 v3 v4 v5 v6 = approxVEq xp164 xp253 &&
			      approxEq' (xp164 ... (vec v1 v2)) ((size xp164) * (size v12))
	  where
		-- approxmate vector equal
		approxVEq (x1, y1, z1) (x2, y2, z2) =
			 and [ approxEq' x1 x2, approxEq' y1 y2, approxEq' z1 z2 ]
		
		-- takes 2 points and returns a vector from the 1st point towards the 2nd point
		vec :: (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float, Float)
		vec (x1, y1, z1) (x2, y2, z2) = (x2 - x1, y2 - y1, z2 - z1)

		-- dot product
		(...) :: (Float, Float, Float) -> (Float, Float, Float) -> Float
		(a1, a2, a3) ... (b1, b2, b3) = a1 * b1 + a2 * b2 + a3 * b3

		-- cross product
		(***) :: (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float, Float)
		(a1, a2, a3) *** (b1, b2, b3) = (a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1)

		-- vector size
		size :: (Float, Float, Float) -> Float
		size (x, y, z) = sqrt $ x^2 + y^2 + z^2

		xp164 = (vec v1 v6) *** (vec v1 v4)
		xp253 = (vec v2 v5) *** (vec v2 v3)
		v12 = (vec v1 v2)

test5 = [
    isValidTP (2,0,0) (2,2,0) (0,2,2) (0,0,2) (-2,2,0) (-2,0,0),
    isValidTP (2,0,0) (2,-2,0) (0,-2,-2) (0,0,-2) (-2,-2,0) (-2,0,0),
    isValidTP (2,0,0) (2,2,0) (0,2,2) (0,0,2) (-2,0,0) (-2,2,0),
    isValidTP (2,0,0) (2,-2,0) (0,-2,-2) (0,0,-2) (-2,0,0) (-2,-2,0),
    isValidTP (0,0,1) (0,1,1) (1,1,0) (1,0,0) (-1,1,0) (-1,0,0)
    ]

