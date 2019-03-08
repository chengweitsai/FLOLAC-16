module Main where
import M8
import Test.QuickCheck

{- Your code here -}

withNext :: [a] -> [(a,a)]
withNext xs = zip xs (tail xs)

eqPrev :: Eq a => a -> [a] -> [(a,a)]
eqPrev x = filter ((x ==) . snd) . withNext

{- Test your code using quickCheck -}

correct0 :: (Int -> [Int] -> [(Int,Int)]) -> Int -> [Int] -> Bool
correct0 f x xs = f x xs == f0 x xs

correct1 :: ([Int] -> [(Int,Int)]) -> [Int] -> Bool
correct1 f xs = f xs == f1 xs


