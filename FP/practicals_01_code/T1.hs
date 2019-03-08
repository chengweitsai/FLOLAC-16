module Main where
import M1
import Test.QuickCheck

main :: IO ()

{- Your code here -}

li xs = [0..(length xs) -1]
g0 xs = [ g1 i xs | i <- li xs ]
g1 i xs = drop i xs ++ take i xs

{- Test your code using quickCheck -}

correct0 :: ([Integer] -> [[Integer]]) -> [Integer] -> Bool
correct0 f xs = f xs == f0 xs

correct1 :: (Int -> [Integer] -> [Integer]) -> Int -> [Integer] -> Bool
correct1 f n xs = f n xs == f1 n xs

main = return ()
