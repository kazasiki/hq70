import           Data.List

match' :: [Int] -> Bool
match' [r, e, a, d, w, i, t, l, k, s] =
  r/=0 && w/=0 && t/=0 && s/=0 && read' + wrire + talk == skill
  where
    read' =             r * 1000 + e * 100 + a * 10 + d
    wrire = w * 10000 + r * 1000 + i * 100 + t * 10 + e
    talk  =             t * 1000 + a * 100 + l * 10 + k
    skill = s * 10000 + k * 1000 + i * 100 + l * 10 + l

filter' :: [[(Char, Int)]]
filter' = map (zip "readwitlks")  $ filter match' $ permutations [0,1,2,3,4,5,6,7,8,9]

main :: IO ()
main = print filter'
