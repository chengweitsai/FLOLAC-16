smaller :: Int -> Int -> Int
smaller x y =
  if x < y then x else y
-- smaller = \x y -> if x < y then x else y
three  :: Int -> Int
three x = 3

infinity :: Int
infinity = 1 + infinity

square :: Int -> Int
square x = x * x

-- #1
even_my :: Int -> Bool
even_my x =
    ( x `mod` 2) == 0
--  ( mod x 2 ) == 0

even_my2 :: Int -> Bool
even_my2 x | ( x `mod` 2) == 0 = True
              -- 1 + ( let y = .... in True)
           | otherwise         = False
            -- where z = .....
-- '| ....' Guarded Expression

-- #2
cirArea :: Int -> Double
cirArea r = pi * fromIntegral(square r)
        where pi :: Double
              pi = 22 / 7
            -- pi * x^2

cirArea2 :: Int -> Double
cirArea2 r = let pi = 22 / 7
             in pi * fromIntegral(square r)

-- #3
payment :: Int -> Int
payment week
  {-let numDays = week * 5
    in (let numHours = numDays*8
       in numHours * 130 )
  -}
  | week > 19 = numHours * 130 * 2
  | otherwise = numHours * 130
  where numDays::Int
        numDays = week * 5
        numHours::Int
        numHours = numDays * 8
-- #4
nested :: Int
nested = let x = 3
          in (let x = 5
              in x + x ) + x
recursive :: Int
recursive = let x = 3
            in let x = x + 1
                in x
-- #5 & #7 & #8
st3  = smaller 3
twice :: (a -> a) -> (a -> a)
      -- (Int -> Int) -> Int -> Int
twice f x = f (f x)
--  twice = f . f

quad :: Int -> Int
quad = (twice square)
--   = square . square
{-
  quad 7
= (twice square) 7
= (square . square) 7
= square( square 7)
= square 7 * square 7
-}

-- #6
poly :: Double -> Double -> Double -> Double -> Double
poly a b c x = a * x^2 + b * x + c

poly1 = poly 1 2 1
poly2 a b c = poly a b c 2

-- # Function Composition
id :: a -> a
id x = x

-- #9
forktimes f g x = f x * g x
poly4 = forktimes (\x -> x+1) (\x -> x+2)
  --  = forktimes (+1) (+2)
-- eata reducitn:   (\x -> e x ) = e
lift2 h f g x = h(f x)(g x)
          --  = forktimes (*)
poly5 = lift2 (*) (\x -> x+1) (\x -> x+2)
  --  = forktimes (*) (+1) (+2)

-- #10

{-
f :: Int -> Char
f = undefined
g1 :: Int -> Char -> Int
g1 = undefined
h :: (Char -> Int) -> Int -> Int
h = undefined
x :: Int
x = undefined
y :: Int
y = undefined
c :: Char
c = undefined
-}

-- positions :: Char -> String -> [Int]
positions x xs =
  map fst (filter (\y -> snd y == x) (zip [0..] xs) )
