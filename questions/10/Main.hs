european :: [Int]
european = [0, 32, 15, 19, 4, 21, 2, 25, 17, 34, 6, 27, 13, 36,
            11, 30, 8, 23, 10, 5, 24, 16, 33, 1, 20, 14, 31, 9,
            22, 18, 29, 7, 28, 12, 35, 3, 26]

american :: [Int]
american = [0, 28, 9, 26, 30, 11, 7, 20, 32, 17, 5, 22, 34, 15,
            3, 24, 36, 13, 1, 00, 27, 10, 25, 29, 12, 8, 19, 31,
            18, 6, 21, 33, 16, 4, 23, 35, 14, 2]

patterns :: [Int] -> Int -> [[Int]]
patterns list n
  | length list < n = []
  | otherwise = take n list : patterns (tail list) n

prepare :: [Int] -> Int -> [[Int]]
prepare list n = patterns (list ++ take (n-1) list) n

calcMax :: [Int] -> Int -> Int
calcMax list n = maximum(map sum $ prepare list n)

compareRoulette :: Int -> Bool
compareRoulette n = calcMax european n < calcMax american n

solve :: Int
solve = length $ filter id $ map compareRoulette [2..36]

main :: IO ()
main = print solve
