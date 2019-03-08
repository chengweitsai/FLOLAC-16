module Main where
import M9
import Test.QuickCheck
main :: IO ()

{- Your code here -}
g0  = map (fst) . g1
g1 xs = filter (\(y,n) ->  n`mod`3 /= 0)
                  (zip xs [0..length xs] )
{- Test your code using quickCheck -}

correct0 :: ([Int] -> [Int]) -> [Int] -> Bool
correct0 f xs = f xs == f0 xs

correct1 :: ([Int] -> [(Int,Int)]) -> [Int] -> Bool
correct1 f xs = f xs == f1 xs

main = return ()
