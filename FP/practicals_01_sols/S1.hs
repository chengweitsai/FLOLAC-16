module Main where
import M1
import Test.QuickCheck

{- Your code here -}

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

rotates :: [a] -> [[a]]
rotates xs = [rotate i xs | i <- [0.. length xs -1]]

{- Test your code using quickCheck -}

correct0 :: ([Integer] -> [[Integer]]) -> [Integer] -> Bool
correct0 f xs = f xs == f0 xs

correct1 :: (Int -> [Integer] -> [Integer]) -> Int -> [Integer] -> Bool
correct1 f n xs = f n xs == f1 n xs
