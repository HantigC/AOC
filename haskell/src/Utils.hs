module Utils where

import qualified Data.Array.IArray as A
import Data.Bifunctor (bimap)
import Data.Char (digitToInt)
import Data.List.Split as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.String (IsString (fromString))
import Text.Printf (printf)

type Coord = (Int, Int)

type GridBounds = ((Int, Int), (Int, Int))

type Grid a = A.Array Coord a

signum x = if x > 0 then 1 else (-1)

inBounds :: Coord -> (Coord, Coord) -> Bool
inBounds (y, x) ((sh, sw), (eh, ew)) = sw <= x && x <= ew && sh <= y && y <= eh

oefBounds :: (Coord, Coord) -> Coord -> Bool
oefBounds ((sh, sw), (eh, ew)) (y, x) =
  sw == x || x == ew || sh == y || y == eh

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

count :: Ord a => [a] -> Map a Int
count = foldl (\m k -> M.insertWith (+) k 1 m) M.empty

getIntList :: [String] -> [Int]
getIntList = foldl (\acc x -> (++) acc $ map read . S.splitOn "," $ x) []

atIxds :: A.Array (Int, Int) a -> [(Int, Int)] -> [a]
atIxds arr idxs = [arr A.! coord | coord <- idxs]

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

crossPattern :: [Coord]
crossPattern =
  [ {-(-1, -1)-}
    (-1, 0 {-(-1, 1)-}),
    (0, -1),
    {-(0, 0)-}
    (0, 1),
    {-( 1, -1)-}
    (1, 0 {-( 1, 1)-})
  ]

squarePattern :: [Coord]
squarePattern =
  [ (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    {-(0, 0)-}
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1)
  ]

computeNeighbCoords :: [Coord] -> Coord -> [Coord]
computeNeighbCoords p (y, x) = map (bimap (+ y) (+ x)) p

computeCrossNeighbCoords :: Coord -> [Coord]
computeCrossNeighbCoords = computeNeighbCoords crossPattern

computeSquareNeighbCoords :: Coord -> [Coord]
computeSquareNeighbCoords = computeNeighbCoords squarePattern

getNeighbours :: [Coord] -> Coord -> A.Array Coord a -> [Coord]
getNeighbours p coord@(y, x) arr =
  filter (`inBounds` bounds) . computeNeighbCoords p $ coord
  where
    bounds = A.bounds arr

getCrossNeighbCoords :: Coord -> A.Array Coord a -> [Coord]
getCrossNeighbCoords = getNeighbours crossPattern

getSquareNeighbCoords :: Coord -> A.Array Coord a -> [Coord]
getSquareNeighbCoords = getNeighbours squarePattern

(==>) :: Monad m => m a -> m b -> m a
x ==> y = do
  result <- x
  y
  return result

convolve :: [a] -> Int -> [[a]]
convolve [] _ = []
convolve xss@(x : xs) n
  | length fstN == n = fstN : convolve xs n
  | otherwise = []
  where
    fstN = take n xss

binToDecStr :: String -> Int
binToDecStr =
  fst
    . foldr
      ((\x (num, cnt) -> (num + x * 2 ^ cnt, cnt + 1)) . digitToInt)
      (0, 0)

binToDec :: [Int] -> Int
binToDec = fst . foldr (\x (num, cnt) -> (num + x * 2 ^ cnt, cnt + 1)) (0, 0)

getResources :: Int -> Int -> String
getResources = printf "../resources/%d/Day%02d.txt"

get2021Resources :: Int -> String
get2021Resources = getResources 2021

get2022Resources :: Int -> String
get2022Resources = getResources 2022


toArray :: [[a]] -> A.Array (Int, Int) a
toArray mapp =
  let h = length mapp
      w = length $ head mapp
   in A.listArray ((0, 0), (h - 1, w - 1)) (concat mapp)
