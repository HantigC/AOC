module Utils where

import Data.Map (Map)
import qualified Data.Map as M
import Data.List.Split as S
import Data.Char (digitToInt)
import qualified Data.Array.IArray as A
import Data.Bifunctor (bimap)


type Coord = (Int, Int)
type GridBounds = ((Int, Int), (Int, Int))

type Grid a = A.Array Coord a

inBounds :: (Coord, Coord)
         -> Coord
         -> Bool
inBounds ((sh, sw), (eh, ew)) (y, x) = sw <= x && x <= ew  && sh <= y && y <= eh


readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

count :: Ord a => [a] -> Map a Int
count = foldl (\m k -> M.insertWith (+) k 1 m) M.empty


getIntList :: [String] -> [Int]
getIntList =  foldl (\acc x -> (++) acc $ map read . S.splitOn "," $ x) []


atIxds :: A.Array (Int, Int) a -> [(Int, Int)] -> [a]
atIxds arr idxs = [arr A.! coord | coord <- idxs]

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

crossPattern :: [Coord]
crossPattern = [ {-(-1, -1)-}  (-1, 0)   {-(-1, 1)-}
                , ( 0, -1), {-(0, 0)-}  ( 0, 1)
                , {-( 1, -1)-}  ( 1, 0)   {-( 1, 1)-}]


squarePattern :: [Coord]
squarePattern = [ (-1, -1),  (-1, 0),   (-1, 1)
                , ( 0, -1), {-(0, 0)-}  ( 0, 1)
                , ( 1, -1),  ( 1, 0),   ( 1, 1)]

computeNeighbCoords :: [Coord]
                    -> Coord
                    -> [Coord]
computeNeighbCoords pattern (y, x) = map (bimap (+y) (+x)) pattern



computeCrossNeighbCoords :: Coord -> [Coord]
computeCrossNeighbCoords = computeNeighbCoords crossPattern


computeSquareNeighbCoords :: Coord -> [Coord]
computeSquareNeighbCoords = computeNeighbCoords squarePattern

getNeighbours :: [Coord]
              -> Coord
              -> A.Array Coord a
              -> [Coord]
getNeighbours pattern coord@(y, x) arr = filter (inBounds bounds)
                                       . computeNeighbCoords pattern
                                       $ coord
  where bounds = A.bounds arr

getCrossNeighbCoords :: Coord
                     -> A.Array Coord a
                     -> [Coord]
getCrossNeighbCoords = getNeighbours crossPattern


getSquareNeighbCoords :: Coord
                     -> A.Array Coord a
                     -> [Coord]
getSquareNeighbCoords = getNeighbours squarePattern


(==>) :: Monad m => m a -> m b -> m a
x ==> y = do
  result <- x
  y
  return result

convolve :: [a] -> Int -> [[a]]
convolve [] _ = []
convolve xss@(x:xs) n
  | length fstN == n = fstN : convolve xs n
  | otherwise = []
  where fstN = take n xss


binToDecStr :: String -> Int
binToDecStr = fst . foldr ((\x (num, cnt) -> (num+x*2^cnt, cnt+1)) . digitToInt) (0, 0)

binToDec :: [Int] -> Int
binToDec = fst . foldr (\x (num, cnt) -> (num+x*2^cnt, cnt+1)) (0, 0)
