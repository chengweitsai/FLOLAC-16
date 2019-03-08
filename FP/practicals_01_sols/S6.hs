module Main where
import M6
import Test.QuickCheck

{- Your code here -}

occurs :: Eq a => a -> [a] -> [a]
occurs x = filter (x==)

count :: Eq a => a -> [a] -> Int
count x xs = length (filter (x ==) xs)

{- Test your code using quickCheck -}

correct0 :: (Int -> [Int] -> Int) -> Int -> [Int] -> Bool
correct0 f x xs = f x xs == f0 x xs

correct1 :: (Int -> [Int] -> [Int]) -> Int -> [Int] -> Bool
correct1 f x xs = f x xs == f1 x xs


