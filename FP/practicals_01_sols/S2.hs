module Main where
import M2
import Test.QuickCheck

{- Your code here -}

rotate2 :: Int -> [a] -> [a]
rotate2 n xs = drop (length xs - n) xs ++ take  (length xs - n) xs

rotates2 :: [a] -> [[a]]
rotates2 xs = [rotate2 i xs | i <- [0.. length xs -1]]

{- Test your code using quickCheck -}

correct0 :: ([Int] -> [[Int]]) -> [Int] -> Bool
correct0 f xs = f xs == f0 xs

correct1 :: (Int -> [Int] -> [Int]) -> Int -> [Int] -> Bool
correct1 f n xs = f n xs == f1 n xs
