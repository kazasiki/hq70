main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  print $ solve n m

solve :: Int -> Int -> Int
solve n m = head [ x | x <- [n+1..], x `mod` m == 0] `div` m + 1
