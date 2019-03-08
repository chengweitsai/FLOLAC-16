module Main where
import M0
import Test.QuickCheck

{- Your code here -}

withNext :: [a] -> [(a,a)]
withNext xs = zip xs (tail xs)

p2l :: (a,a) -> [a]
p2l (x,y) = [x,y]

adj = concat . map p2l . withNext

{- Test your code using quickCheck -}

correct0 :: ([Int] -> [Int]) -> [Int] -> Bool
correct0 f xs = f xs == f0 xs

correct1 :: ((Int, Int) -> [Int]) -> (Int, Int) -> Bool
correct1 f (x,y) = f (x,y) == f1 (x,y)

correct2 :: ([Int] -> [(Int, Int)]) -> [Int] -> Bool
correct2 f xs = f xs == f2 xs