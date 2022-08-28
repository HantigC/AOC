module DayFive where
import qualified Data.List.Split as S
import qualified Utils as U
import qualified Data.Map as M


data Venture = Venture { st :: (Int , Int) , nd :: (Int , Int) } deriving (Show)


instance Read Venture where
  readsPrec _ s = [(Venture { st = (read x1, read y1), nd = (read x2, read y2)} , "")]
    where ((x1:y1:_):(x2:y2:_):_) = map (S.splitOn ",") . S.splitOn " -> " $ s


play :: [Venture] -> Int
play = length . M.filter (> 1) . foldl store M.empty
  where
    store m v = M.unionWith (+) m . M.fromList . zip (expand v) $ repeat 1


expand :: Venture -> [(Int, Int)]
expand (Venture (x1, y1) (x2, y2))
  | x1 == x2 = zip (repeat x1) $ unwrap y1 y2
  | y1 == y2 = zip (unwrap x1 x2) $ repeat y1
  | otherwise = zip (unwrap x1 x2) (unwrap y1 y2)
    where unwrap b e = if b < e then [b..e] else [b,b-1..e]


isStraight :: Venture -> Bool
isStraight (Venture (x1, y1) (x2, y2)) = x1 == x2 || y1 == y2


datas = [ "0,9 -> 5,9"
        , "8,0 -> 0,8"
        , "9,4 -> 3,4"
        , "2,2 -> 2,1"
        , "7,0 -> 7,4"
        , "6,4 -> 2,0"
        , "0,9 -> 2,9"
        , "3,4 -> 1,4"
        , "0,0 -> 8,8"
        , "5,5 -> 8,2"
        ]


main :: IO ()
main = do
  lines <- U.readLines "../resources/day_5.txt"
  print "First"
  print $ play . filter isStraight . map read $ lines
  print "Second"
  print $ play . map read $ lines

