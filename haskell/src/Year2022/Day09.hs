module Day09 where

import Data.Either
import Data.List (intercalate, nub)
import qualified Data.Set as S
import Text.ParserCombinators.Parsec
import Utils as U

data Direction = U | D | L | R deriving (Show, Eq)

directionFromStr 'R' = R
directionFromStr 'U' = U
directionFromStr 'L' = L
directionFromStr 'D' = D
directionFromStr _ = error "Direction should be L,R,U,D"

parseMove = do
  direction <- anyChar
  char ' '
  qty <- many digit
  optional (char '\n')
  return $ replicate (read qty) $ directionFromStr direction

nextState :: U.Coord -> U.Coord -> U.Coord
nextState h@(hx, hy) t@(tx, ty)
  | dx' == 2 && dy' == 0 = (tx + dx'', ty)
  | dx' == 0 && dy' == 2 = (tx, ty + dy'')
  | dx' == 2 && dy' >= 1 = (tx + dx'', ty + dy'')
  | dx' >= 1 && dy' == 2 = (tx + dx'', ty + dy'')
  | otherwise = t
  where
    dx'' = U.signum dx
    dy'' = U.signum dy
    dx' = abs dx
    dy' = abs dy
    dx = hx - tx
    dy = hy - ty

move :: U.Coord -> Direction -> U.Coord
move h@(hx, hy) direction
  | direction == R = (hx + 1, hy)
  | direction == L = (hx -1, hy)
  | direction == D = (hx, hy -1)
  | otherwise = (hx, hy + 1)

moveRope :: [U.Coord] -> Direction -> [U.Coord]
moveRope [] _ = []
moveRope (x : xss) didrection = f (move x didrection : xss)
  where
    f [] = []
    f [x] = [x]
    f (x1 : x2 : xs) = let newTail = nextState x1 x2 in (x1 : f (newTail : xs))

parseMoves = concat <$> many parseMove

example = intercalate "\n" ["R 4", "U 4", "L 3", "D 1", "R 4", "D 1", "L 5", "R 2"]

example' = intercalate "\n" ["R 5", "U 8", "L 8", "D 3", "R 17", "D 10", "L 25", "U 20"]

solve :: Int -> String -> Int
solve n str =
  let directions = fromRight [] $ parse parseMoves "" str
   in length . nub . map last . scanl moveRope (replicate n (0, 0)) $ directions

main = do
  input <- readFile $ get2022Resources 9
  print $ parse parseMoves "" example
  print $ solve 2 example
  print $ solve 2 input
  print $ solve 10 example'
  print $ solve 10 input