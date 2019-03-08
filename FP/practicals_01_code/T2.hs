module Main where
import M2
import Test.QuickCheck
main :: IO ()

{- Your code here -}
g0 xs = [g1 i xs | i <- [0..length xs - 1]]
g1 i xs = drop (length xs - i) xs ++ take (length xs - i) xs
{- Test your code using quickCheck -}

correct0 :: ([Int] -> [[Int]]) -> [Int] -> Bool
correct0 f xs = f xs == f0 xs

correct1 :: (Int -> [Int] -> [Int]) -> Int -> [Int] -> Bool
correct1 f n xs = f n xs == f1 n xs

main = return ()
