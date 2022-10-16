module Day17 where


import Text.ParserCombinators.Parsec hiding (between)
import Data.List (nub, intercalate, transpose, maximumBy, sort)
import Data.Char (digitToInt)
import Data.MemoTrie (memo)
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Array.IArray as A
import Data.Maybe (isNothing, fromJust, isJust)
import qualified Data.Map as M

import qualified Utils as U

example = "target area: x=20..30, y=-10..-5"

parseTargetArea :: GenParser Char st (Int, Int, Int, Int)
parseTargetArea = do
  string "target area: "
  string "x="
  xMin <- manyTill anyChar (string "..")
  xMax <- manyTill anyChar (string ", ")
  string "y="
  yMin <- manyTill anyChar (string "..")
  yMax <- many anyChar

  return (read xMin :: Int, read xMax :: Int, read yMin :: Int, read yMax :: Int)


between x xMin xMax = x >= xMin && x <= xMax

maxHeight (xLoc, yLoc) (xMin, xMax, yMin, yMax)
  | yLoc > yMax = Just $ sumToN . abs $ yMin + 1
  | yLoc < yMin = Just $ sumToN yMax
  | otherwise = Nothing


sumToN n = n * (n+1) `div` 2

getYBods :: Int -> (Int, Int)
getYBods minY = (minY, abs minY - 1)


yeah vxs vys bnds = [move (0,0,vx,vy) bnds |  vy <- vys, vx <- vxs]


nextBounds xMin xMax = takeWhile (/= 0) $ f xMin xMax 1
  where f xMin xMax n = [xMin `div` n .. xMax `div` n] ++ f xMin xMax (n+1)

gitIt bnds@(xMin, xMax, yMin, yMax) =  nub . filter isJust . yeah (nextBounds xMin xMax) [yMin..(abs yMin - 1)] $ bnds


move state@(x, y, vx, vy) (xMin, xMax, yMin, yMax) = do
  f state 0
  where f s@(x, y, vx, vy) maxY
          | x > xMax = Just s
          | y < yMin = Just s
          | between x xMin xMax && between y yMin yMax = Just s
          | otherwise = f (x+vx, y+vy, f' vx, vy-1) (max y maxY)
        f' vx
          | vx < 0 = vx + 1
          | vx > 0 = vx - 1
          | otherwise = 0

showTarget :: (Int, Int, Int, Int) -> String
showTarget (xMin, xMax, yMin, yMax) = undefined
  where rightBound = maximum [0, abs xMin, abs xMax]
        leftBound = minimum [0, abs xMin, abs xMax]
        upperBound = maximum [0, abs yMin, abs yMax]
        lowerBound = minimum [0, abs yMin, abs yMax]


partOne str = do
  bnds <- parse parseTargetArea "" str
  return $ maxHeight (0,0) bnds


partTwo str = do
  bnds <- parse parseTargetArea "" str
  return $ gitIt bnds


main = do
  input <- readFile "../resources/Day17.txt"
  print $ partOne example
  print $ partOne input

  print $ partTwo example
  print $ partTwo input
