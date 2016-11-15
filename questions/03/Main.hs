import           Data.List

main :: IO ()
main = print solve

solve :: [Int]
solve = filter check [1..100]

check :: Int -> Bool
check n = foldl' (\x y -> if n `mod` y == 0 then not x else x) False [1..100]
