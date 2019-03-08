module Main where
import M0
import Test.QuickCheck
main :: IO ()
{- Your code here -}

withNext xs = zip xs (tail xs)
g0  = concat . map g1 . withNext  
g1 (x, y) = [x,y]
{- Test your code using quickCheck -}

correct0 :: ([Int] -> [Int]) -> [Int] -> Bool
correct0 f xs = f xs == f0 xs

correct1 :: ((Int, Int) -> [Int]) -> (Int, Int) -> Bool
correct1 f (x,y) = f (x,y) == f1 (x,y)

correct2 :: ([Int] -> [(Int, Int)]) -> [Int] -> Bool
correct2 f xs = f xs == f2 xs

main = return ()
