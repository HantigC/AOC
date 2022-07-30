import Utils as U
import qualified Data.Map as M
import Data.Char (digitToInt)
import Control.Monad (mapM, foldM)
import qualified Data.Array.IArray as A
import Data.List (sort)


type TextList = [String]

data EnergyLevel = Level Int
                 | Flashed deriving (Show, Eq, Ord)

type Grid a = A.Array U.Coord a

type EnergyGrid = Grid EnergyLevel


energyLevels = [ "5483143223"
               , "2745854711"
               , "5264556173"
               , "6141336146"
               , "6357385478"
               , "4167524645"
               , "2176841721"
               , "6882881134"
               , "4846848554"
               , "5283751526"
               ]


smallEnergyLevels = [ "11111"
                    , "19991"
                    , "19191"
                    , "19991"
                    , "11111"
                    ]


textListToArray :: (Char -> a) -> TextList -> Grid a
textListToArray f xss = A.listArray idx [f x | xs <- xss, x <- xs]
  where idx = ((0, 0), (length xss - 1, length (head xss) - 1))


computeFlashNumber :: Int -> EnergyGrid ->  Int
computeFlashNumber num grid = snd $ foldl f (grid, 0) [1..num]
  where f (grid, flashes) _ = (A.amap f grid', flashes + flashes')
          where grid' = fillOneStep grid
                flashes' = foldl f' 0 grid'

                f' x Flashed = x + 1
                f' x _ = x

                f Flashed =  Level 0
                f x = x


getFullFlashes :: EnergyGrid -> Int
getFullFlashes = getFullFlashes' 0
  where getFullFlashes' num grid =
          if allFlashed
             then currFlash
             else getFullFlashes' currFlash $ A.amap f grid'

          where grid' = fillOneStep grid
                allFlashed = all (==Flashed) grid'
                currFlash = num + 1

                f Flashed = Level 0
                f x = x


bfs :: EnergyGrid -> [U.Coord] -> EnergyGrid
bfs grid []  = grid
bfs grid (coord@(y, x) : coords) = f el
  where el = grid A.! coord
        f (Level x)
          | x < 9 = bfs (grid A.// [(coord, Level (x+1))]) coords
          | otherwise = bfs (grid A.// [(coord, Flashed)]) (neighCoords ++ coords)
          where neighCoords = U.getSquareNeighbCoords coord grid

        f Flashed = bfs grid coords


fillOneStep :: EnergyGrid -> EnergyGrid
fillOneStep energyLevels = energyLevels'
  where ((sh, sw), (eh, ew)) = A.bounds energyLevels
        s = [(i, j) | i <- [sh..eh], j <- [sw..ew]]
        energyLevels' = bfs energyLevels s




partOne :: Int -> TextList -> Int
partOne num strs = computeFlashNumber num energyLevels
  where energyLevels = textListToArray f strs
        ((sh, sw), (eh, ew)) = A.bounds energyLevels
        f x = Level $ digitToInt x


partTwo :: TextList -> Int
partTwo strs = getFullFlashes $ textListToArray (Level .  digitToInt)  strs


main :: IO ()
main = do
  lines <- U.readLines "resources/day_11.txt"
  print $ partOne 100 energyLevels
  print $ partOne 100 lines
  print $ partTwo energyLevels
  print $ partTwo lines
