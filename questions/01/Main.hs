main :: IO ()
main = print solve

solve :: Int
solve = head palindromics

palindromics :: [Int]
palindromics = [ x |
                  x <- [10..],
                  isPalindromic $ int2bin x,
                  isPalindromic $ int2oct x,
                  isPalindromic $ int2dec x
                  ]

isPalindromic :: [Int] -> Bool
isPalindromic ns = ns == reverse ns

int2bin :: Int -> [Int]
int2bin = int2base 2

int2oct :: Int -> [Int]
int2oct = int2base 8

int2dec :: Int -> [Int]
int2dec = int2base 10

int2base :: Int -> Int -> [Int]
int2base _ 0 = []
int2base x y = y `mod` x : int2base x (y `div` x)
