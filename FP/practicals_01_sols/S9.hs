module Main where
import M9
import Test.QuickCheck

{- Your code here -}


idx1 :: [a] -> [(a,Int)]
idx1 xs = filter (\(x,n) -> n `mod` 3 /= 0)
            (zip xs [0..])

idx2 :: [a] -> [a]
idx2 = map fst . idx1

{- Test your code using quickCheck -}

correct0 :: ([Int] -> [Int]) -> [Int] -> Bool
correct0 f xs = f xs == f0 xs

correct1 :: ([Int] -> [(Int,Int)]) -> [Int] -> Bool
correct1 f xs = f xs == f1 xs
