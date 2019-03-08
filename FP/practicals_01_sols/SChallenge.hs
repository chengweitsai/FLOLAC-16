module Main where
import MChallenge
import Test.QuickCheck

{- Your code here -}

find' :: Eq a => [a] -> [a] -> [a]
find' xs ys = head (filter (\zs -> xs == take (length xs) zs) (tails ys))

tails :: [a] -> [[a]]
tails = foldr (\x xss -> (x : head xss) : xss) [[]] 

{- Equivalently:

lens :: [a] -> [Int]
lens xs = [i | i <- [0..length xs]]

tails :: [a] -> [[a]]
tails xs = map (\i -> drop i xs) (lens xs)

-}

{- Test your code using quickCheck -}

correct0 :: ([Int] -> [Int] -> [Int]) -> [Int] -> [Int] -> Bool
correct0 f xs ys = f xs ys == find xs ys

