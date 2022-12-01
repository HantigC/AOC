module Day17 where


import           Data.List                      ( nub )
import           Data.Maybe                     ( isJust )
import           Text.ParserCombinators.Parsec
                                         hiding ( between )
import qualified Utils                         as U

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

  return
    (read xMin :: Int, read xMax :: Int, read yMin :: Int, read yMax :: Int)

between x xMin xMax = x >= xMin && x <= xMax

maxHeight (xLoc, yLoc) (xMin, xMax, yMin, yMax)
  | yLoc > yMax = Just $ sumToN . abs $ yMin + 1
  | yLoc < yMin = Just $ sumToN yMax
  | otherwise   = Nothing

sumToN n = n * (n + 1) `div` 2


moves vxs vys bnds = [ move (0, 0, vx, vy) bnds | vy <- vys, vx <- vxs ]

move state@(x, y, vx, vy) (xMin, xMax, yMin, yMax) = do
  f state 0
 where
  f s@(x, y, vx, vy) maxY
    | x > xMax  = Nothing
    | y < yMin  = Nothing
    | between x xMin xMax && between y yMin yMax = Just s
    | otherwise = f (x + vx, y + vy, f' vx, vy - 1) (max y maxY)
  f' vx | vx < 0    = vx + 1
        | vx > 0    = vx - 1
        | otherwise = 0

partOne str = do
  bnds <- parse parseTargetArea "" str
  return $ maxHeight (0, 0) bnds


partTwo str = do
  bnds@(xMin, xMax, yMin, yMax) <- parse parseTargetArea "" str
  return
    $ length
    . nub
    . filter isJust
    . moves [0 .. xMax] [yMin .. (abs yMin - 1)]
    $ bnds


main = do
  input <- readFile $ U.get2021Resources 17
  print $ partOne example
  print $ partOne input

  print $ partTwo example
  print $ partTwo input
