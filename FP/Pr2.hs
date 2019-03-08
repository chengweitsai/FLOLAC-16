-- #2 : prove: sum . concat = sum . map sum

{-
-- IB:  case []
sum (concat []) = sum ([]) = 0
sum (map sum [])  = sum ([])
-- IH: case x:xs
  sum (concat x:xs )
  {-def of concat-}
= sum ( x ++ concat xs)
  {-def of sum and ++ -}
= sum x + sum( concat xs)
  {-induction-}
= sum x + sum . map (sum xs)
  {-def of sum-}
= sum ( sum x : map (sum xs) )
  {-def of map-}
= sum( map (sum x:xs) )
-}

-- #4
fan :: a -> [a] -> [[a]]
fan y [] = [[y]]
fan y (x:xs) = (y:(x:xs)): (map (x:) (fan y xs) )

-- #5
{-
delete x xs = filter (\i -> i /= x) xs -- auxilary

perms :: [a] -> [[a]]
perms [] = [[]]
perms xs = [x:ys | x<- xs, ys <- perms (delete x xs)]

--perms (x:xs) = [x : perms xs | i <- ]
-- perms  (x:xs) = fan x xs ++ fan x (reverse xs)
-- perms (x:xs) =  concat (map (fan x) (perms xs ) )
-}


-- Extra Practice
-- subsets [1,2] = [ [], [1], [2], [1,2]]
subsets [] = [[]]
--subsets (x:xs) = [map (x ++) subsets(xs) ]
