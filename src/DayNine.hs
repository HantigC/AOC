module DayNine where

import Data.Map (Map)
import Data.List (sort, transpose)
import Data.Char (digitToInt)
import Control.Monad (mapM)
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


selectValeys :: (Ord a, Num a) => [[[[a]]]] -> [[a]]
selectValeys = map (map f)
  where f ( (_  :x01:_)
           :(x10:x11:x12:_)
           :(_  :x21:_):_) = if all (> x11) [x01, x10, x12, x21]
                                then x11 + 1
                                else 0
        f _ = error "waiting 3X3 window"


partOne :: [[Char]] -> Int
partOne xs = sum2d
            . selectValeys
            . chunk2d (3, 3)
            . pad2d 10
            $ map2d digitToInt xs


mapp = [ "2199943210"
       , "3987894921"
       , "9856789892"
       , "8767896789"
       , "9899965678"
       ]

main :: IO ()
main = do
  lines <- U.readLines "resources/day_9.txt"
  print $ partOne mapp
  print $ partOne lines

