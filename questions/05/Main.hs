main :: IO ()
main = print $ length solve

solve :: [(Int, Int, Int, Int)]
solve = [ (y10, y50, y100, y500) |
          y10  <- [0..15],
          y50  <- [0..15],
          y100 <- [0..15],
          y500 <- [0..15],
          y10 * 10 + y50 * 50 + y100 * 100 + y500 * 500 == 1000,
          y10 + y50 + y100 + y500 <= 15
          ]
