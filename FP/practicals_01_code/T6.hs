module Main where
import M6
import Test.QuickCheck
main :: IO ()
{- Your code here -}

g0 x xs = length  (g1 x xs)
g1 x  = filter (x==)
{- Test your code using quickCheck -}

correct0 :: (Int -> [Int] -> Int) -> Int -> [Int] -> Bool
correct0 f x xs = f x xs == f0 x xs

correct1 :: (Int -> [Int] -> [Int]) -> Int -> [Int] -> Bool
correct1 f x xs = f x xs == f1 x xs

main = return ()
