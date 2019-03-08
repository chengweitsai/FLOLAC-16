module Main where
import M7
import Test.QuickCheck

{- Your code here -}

withNext :: [a] -> [(a,a)]
withNext xs = zip xs (tail xs)

eqNext :: Eq a => a -> [a] -> [(a,a)]
eqNext x = filter ((x ==) . fst) . withNext

{- Test your code using quickCheck -}

correct0 :: (Int -> [Int] -> [(Int,Int)]) -> Int -> [Int] -> Bool
correct0 f x xs = f x xs == f0 x xs

correct1 :: ([Int] -> [(Int,Int)]) -> [Int] -> Bool
correct1 f xs = f xs == f1 xs


