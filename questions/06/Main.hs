main :: IO ()
main = print $ length solve

solve :: [Int]
solve = [ x | x <- [0..10000], even x, check x]

check :: Int -> Bool
check n = elem n $ myCollatz n

myCollatz :: Int -> [Int]
myCollatz n | even n    = collatz (n * 3 + 1)
            | otherwise = collatz n

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n | even n    = n : collatz (n `div` 2)
          | otherwise = n : collatz (n * 3 + 1)
