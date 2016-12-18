{-# LANGUAGE FlexibleInstances #-}
import           Data.Monoid

type Pos = (Int, Int)
type Route = [Pos]

class Monoid r => Routable r where runit :: Route -> r
instance Routable [Route] where runit route = [route]
instance Routable (Sum Int) where runit _ = Sum 1

mroutes :: Routable r => Int -> Route -> r
mroutes 0 route = runit route
mroutes i route = mconcat $ map (mroutes (i - 1) . (:route)) $ nextPoss route

nextPoss :: Route -> [Pos]
nextPoss [] = [(0, 0)]
nextPoss route@((x,y):_) = filter (not . (`elem` route)) candidates
                           where candidates = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

count :: Int -> Route -> Sum Int
count = mroutes

main :: IO ()
main = print $ count 12 [(0, 0)]
