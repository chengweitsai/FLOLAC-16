doubleAll :: [Int] -> [Int]
doubleAll xs = map (\x -> x * 2) xs
            -- = map (2*) xs

quadAll :: [Int] -> [Int]
quadAll  = doubleAll . doubleAll


squareUpto :: Int -> [Int]
squareUpto x = filter ( < x ) [ x*x | x <- [0..x] ]

-- positions :: Char -> String -> [Int]
positions x xs =
  map fst (filter (\y -> snd y == x) (zip [0..] xs) )

pos x xs  = head (positions x xs)

-- # foldr
prod :: [Int] -> Int
prod xs = foldr (*) 1 xs

myMaximum :: [Int] -> Int
myMaximum xs = foldr max minBound xs

myLength :: [a] -> Int
myLength xs =  foldr (\x n -> n + 1) 0 xs
