fib :: Int -> Integer
fib n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

int2dec :: Integer -> [Integer]
int2dec = int2base 10

int2base :: Integer -> Integer -> [Integer]
int2base _ 0 = []
int2base x y = y `mod` x : int2base x (y `div` x)

check :: Integer -> Bool
check n = n `mod` sum (int2dec n) == 0

solve :: [Integer]
solve = take 5 $ filter check $ map fib [13..]

main :: IO ()
main = print solve
