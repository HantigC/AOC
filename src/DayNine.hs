module DayNine where

import Data.Map (Map)
import Data.List (sort, transpose)
import Data.Char (digitToInt)
import Control.Monad (mapM)
import qualified Data.Ix as Ix
import qualified Data.Array as A
import qualified Data.Set as Set
import qualified Data.Map as M
import qualified Utils as U
import qualified Data.List.Split as S
import qualified Control.Monad as CM


chunk1d :: Int -> [a] -> [[a]]
chunk1d _ [] = []
chunk1d winSize xss@(x:xs)
  | length win == winSize = win : chunk1d winSize xs
  | otherwise = []
  where win = take winSize xss


chunk2d :: (Int, Int) -> [[a]] -> [[[[a]]]]
chunk2d (w, h) xs = twoChunks
  where oneChunks = map (chunk1d  w) xs
        twoChunks = map (chunk1d h) .transpose $ oneChunks



pad1d :: a -> [a] -> [a]
pad1d x xs = x : p xs
  where p [] = [x]
        p (s:ss) = s : p ss

pad2d :: a -> [[a]] -> [[a]]
pad2d x = transpose . map (pad1d x) . transpose . map (pad1d x)


sum2d :: Num a => [[a]] -> a
sum2d = sum . map sum

map2d :: (a -> b) -> [[a]] -> [[b]]
map2d f = map $ map f


selectValeys :: (Ord a, Num a) => [[[[a]]]] -> Maybe [[a]]
selectValeys xs = mapM (mapM f) xs
  where f ( (_  :x01:_)
           :(x10:x11:x12:_)
           :(_  :x21:_):_) = if all (> x11) [x01, x10, x12, x21]
                                then Just (x11 + 1)
                                else Just 0
        f _ = Nothing
        partia = map (map f) xs


partOne :: [[Char]] -> Maybe Int
partOne xs = do
  let digits = map2d digitToInt xs
      paddedDigits = pad2d 10 digits
      chunkedDigits = chunk2d (3, 3) paddedDigits
  valeys <- selectValeys chunkedDigits
  Just $ sum2d valeys


mapp = [ "2199943210"
       , "3987894921"
       , "9856789892"
       , "8767896789"
       , "9899965678"
       ]

atIxds :: A.Array (Int, Int) Int -> [(Int, Int)] -> [Int]
atIxds arr idxs = [arr A.! coord | coord <- idxs]


enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]


bazinLineToArray :: [String] -> A.Array (Int, Int) Int
bazinLineToArray xss = A.array idx elements
  where idx = ((0, 0), (length xss - 1, length (head xss) - 1))
        elements = [((i, j), digitToInt x)
          | (i, xs) <- enumerate xss, (j, x) <- enumerate xs]


findSmokeBasin :: A.Array (Int, Int) Int -> [Maybe (Int, Int)]
findSmokeBasin arr = filter (/= Nothing) [f (y, x) | y <- [0..h] , x <- [0..w]]
  where (_, (h, w)) = A.bounds arr
        inBounds (y, x) = 0 <= x && x <= w  && 0 <= y && y <= h
        getIfInBounds coord
          | inBounds coord = Just $ arr A.! coord
          | otherwise = Nothing

        f (y, x) = if all (> center) neighbours
                      then Just (y, x)
                      else Nothing
          where up = getIfInBounds (y - 1, x)
                down = getIfInBounds (y + 1, x)
                left = getIfInBounds (y, x - 1)
                right = getIfInBounds (y, x + 1)
                neighbours = filter (/= Nothing) [down, up, left, right]
                center = getIfInBounds (y, x)

partOne' :: [String] -> Maybe Int
partOne' strBasins = do
  basins <- sequence smokeBasins
  return $ sum . map (+1) . atIxds mapp $ basins
  where mapp = bazinLineToArray strBasins
        smokeBasins = findSmokeBasin mapp



main :: IO ()
main = do
  lines <- U.readLines "resources/day_9.txt"
  print $ partOne mapp
  print $ partOne lines
  print $ partOne' mapp
  print $ partOne' lines
  -- print $ bazinLineToArray lines

