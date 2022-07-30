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


textListToArray :: (Char -> a)
                -> TextList
                -> Grid a
textListToArray f xss = A.listArray idx [f x | xs <- xss, x <- xs]
  where idx = ((0, 0), (length xss - 1, length (head xss) - 1))


partOne :: Int -> TextList -> Int
partOne num strs = snd $ fillSteps num energyLevels
  where energyLevels = textListToArray f strs
        ((sh, sw), (eh, ew)) = A.bounds energyLevels
        f x = Level $ digitToInt x


fillSteps :: Int -> EnergyGrid -> (EnergyGrid, Int)
fillSteps num grid = foldl f (grid, 0) [1..num]
  where f (grid, flashes) _ = (grid', flashes + flashes')
          where (grid', flashes') = fillOneStep grid

fillOneStep :: EnergyGrid -> (EnergyGrid, Int)
fillOneStep energyLevels = (A.amap f energyLevels', flashes)
  where ((sh, sw), (eh, ew)) = A.bounds energyLevels
        s = [(i, j) | i <- [sh..eh], j <- [sw..ew]]

        energyLevels' = bfs energyLevels s
        flashes = foldl f' 0 energyLevels'

        f' x Flashed = x + 1
        f' x _ = x

        f Flashed = f $ Level 0
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


main :: IO ()
main = do
  lines <- U.readLines "resources/day_11.txt"
  print $ partOne 100 energyLevels
  print $ partOne 100 lines
