module Main where
import M3
import Test.QuickCheck
main :: IO ()
{- Your code here -}

g0 xs = concat [xs | i <- g1 xs]
-- concat (map (const xs) (g1 xs))
g1 xs = [0..length xs]

{- Test your code using quickCheck -}

correct0 :: ([Int] -> [Int]) -> [Int] -> Bool
correct0 f xs = f xs == f0 xs

correct1 :: ([Int] -> [Int]) -> [Int] -> Bool
correct1 f xs = f xs == f1 xs

main = return ()
