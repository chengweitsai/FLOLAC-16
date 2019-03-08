square :: Int -> Int
square x = x * x

smaller :: Int -> Int -> Int
smaller x y =
  if x < y then x else y

three :: Int -> Int
three x = 3

infinity :: Int
infinity = 1 + infinity

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

cirArea :: Int -> Double
cirArea r = pi * fromIntegral(square r)
        where pi :: Double
              pi = 22 / 7
            -- pi * x^2

cirArea2 :: Int -> Double
cirArea2 r = let pi = 22 / 7
             in pi * fromIntegral(square r)
