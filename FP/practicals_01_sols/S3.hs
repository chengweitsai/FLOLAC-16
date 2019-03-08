module Main where
import M3
import Test.QuickCheck

{- Your code here -}


lens :: [a] -> [Int]
lens xs = [0..length xs]

repeatN :: [a] -> [a]
repeatN xs = concat (map (const xs) (lens xs))

{- Test your code using quickCheck -}

correct0 :: ([Int] -> [Int]) -> [Int] -> Bool
correct0 f xs = f xs == f0 xs

correct1 :: ([Int] -> [Int]) -> [Int] -> Bool
correct1 f xs = f xs == f1 xs
