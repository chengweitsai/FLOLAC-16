sum_my :: [Int] -> Int
sum_my [] = 0
sum_my (x : xs) = x + sum_my xs

map_my :: (a -> b) -> [a] -> [b]
map_my f [] = []
map_my f (x : xs) = f x : map_my f xs

concat_my :: [[a]] -> [a]
concat_my [] = []
concat_my (xs : xss) = xs ++ concat_my(xss)

length_my :: [a] -> Int
length_my [] = 0
length_my (x:xs) = 1 + length_my xs

filter_my :: (a -> Bool) -> [a] -> [a]
filter_my p [] = []
filter_my p (x:xs) | p x = x : filter_my p xs
                   | otherwise = filter_my p xs

take_my :: Int -> [a] -> [a]
take_my 0 xs = []
take_my n [] = []
take_my n (x:xs) = x : take_my (n-1) xs

drop_my :: Int -> [a] -> [a]
drop_my 0 xs = xs
drop_my n [] = []
drop_my n (x:xs) = drop_my (n-1) xs

takeWhile_my :: (a -> Bool) -> [a] -> [a]
takeWhile_my p [] = []
takeWhile_my p (x:xs) | p x = x : takeWhile_my p xs
                      | otherwise = []

dropWhile_my :: (a -> Bool) -> [a] -> [a]
dropWhile_my p [] = []
dropWhile_my p (x:xs) | p x = dropWhile_my p xs
                      | otherwise = x:xs

reverse_my :: [a] -> [a]
reverse_my [] = []
reverse_my (x:xs) = reverse_my xs ++ [x]

inits_my :: [a] -> [[a]]
inits_my [] = [[]]
inits_my (x:xs) = [] : map (x:) (inits_my xs)

tails_my :: [a] -> [[a]]
tails_my [] = [[]]
tails_my (x:xs) = (x:xs) : tails_my xs

merge_my :: [Int] -> [Int] -> [Int]
merge_my [] [] = []
merge_my (x:xs) [] = x:xs
merge_my [] (y:ys) = y:ys
merge_my (x:xs) (y:ys) | x <= y    = x: merge_my xs (y:ys)
                       | otherwise = y: merge_my (x:xs) ys

zip_my :: [a] -> [b] -> [(a,b)]
zip_my [] [] = []
zip_my [] (y:ys) = []
zip_my (x:xs) [] = []
zip_my (x:xs) (y:ys) = (x,y) : zip_my xs ys

-- unzip_my [ (1, 'a'), (2, 'b')] = ([1,2], "ab")
unzip_my :: [(a,b)] -> ([a],[b])
unzip_my [] = ([], [])
--unzip_my (x,y):l = ( x:xs , y:ys )
    --where (xs, ys) = unzip_my l
-- pattern matching at return value

msort_my ::[Int] -> [Int]
msort_my [] = []
msort_my [x] = [x]
msort_my xs = merge_my (msort_my ys) (msort_my zs)
  where n = length_my xs `div` 2
        ys = take n xs
        zs = drop n xs


data Tree a = Null | Node a (Tree a) (Tree a)
sumT :: Tree Int -> Int
sumT Null = 0
sumT (Node x t u) = x + sumT t + sumT u

minT :: Tree Int -> Int
minT Null = maxBound :: Int
minT (Node x t u) = min (min x (minT t)) (minT u)

mapT :: (a -> b) -> Tree a -> Tree b
mapT f Null = Null
mapT f (Node x t u) = Node (f x) (mapT f t) (mapT f u)
{-
test case:
a = Node 4 Null Null
b = Node 9 Null Null
c = Node 7 Null Null
d = Node 10 a b
e = Node 7 d c
-}


-- IH : minT . mapT (n+) = (n+) minT
{-
  minT . mapT (n+) (Node x t u)
= minT (Node (n+x) (mapT (n+) t ) (mapT (n+) u) )
  {- def of minT-}
= min ( n+x, min( minT(mapT (n+) t), minT(mapT (n+) u) ) )
  {- induction -}
= min (  n+x, min( (n+) minT(t), (n+) minT(u)  ))
= min (  n+x, (n+) min( minT(t), minT(u) ) )
= (n+) min( x, min( minT(t), minT(u) ) )
  {- def of minT-}
= (n+) minT(Node x t u)
-}
