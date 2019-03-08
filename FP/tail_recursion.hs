-- normal
-- fac n | n == 0  = 1
-- fac n | n > 0   = n * fac (n-1)

-- tail recursion: recursive call occurs last
-- accumulating parameters  累積參數
fac n = tailfac n 1 --
tailfac n acc | n==0 = acc
              | n>0  = tailfac (n-1) n *acc
-- 中間就開始做乘法, stack 不會一直長, fixed space

-- Fibonacci Numbers
-- fib n | n > 1 = fib (n-1) + fib (n-2)
-- fib 1 = 1
-- fib 0 = 0

fib n = tailFib n 0 1
tailFib :: Int -> Int -> Int -> Int
tailFib n a b | n ==  0 = a
              | n ==  1 = b
              | otherwise = tailFib (n-1) b (a+b)

-- 0 1 1 2 3 5 ...
-- a b
--   a b

-- /////////////////////////////////////////////
reverse_my :: [a] -> [a]
reverse_my xs = tailRev xs []
tailRev :: [a] -> [a] -> [a]
tailRev [] ys = ys
tailRev (x:xs) ys = tailRev xs (x:ys)
