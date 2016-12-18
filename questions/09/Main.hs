patterns :: Int -> Int -> Int -> Int -> Int
patterns 0 0 _ _ = 1
patterns a b c d
  | a == b || (c-a) == (d-b) = 0
  | otherwise = (if a > 0 then patterns (a-1) b c d else 0) + (if b > 0 then patterns a (b-1) c d else 0)

solve :: Int -> Int -> Int
solve a b = patterns (a-1) b a b + patterns a (b-1) a b

main :: IO ()
main = print $ solve 20 10
