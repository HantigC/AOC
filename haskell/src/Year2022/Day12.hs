module Year2022.Day12 where

import qualified Data.Array.IArray as A
import Data.Char (ord)
import Data.List (intercalate)
import PathFinding (findMinPathDijkstra)
import qualified Utils as U

example =
  intercalate
    "\n"
    [ "Sabqponm",
      "abcryxxl",
      "accszExk",
      "acctuvwj",
      "abdefghi"
    ]

toArray = U.toArray . map (map ord) . lines

fNeighbours grid coord =
  let neighbs = U.getCrossNeighbCoords coord grid
      currVal = grid A.! coord
      s = zip (grid `U.atIxds` neighbs) neighbs
   in map snd . filter (\(v, c) -> (v + 1) >= currVal) $ s

fCost coord@(cx, cy) = 1

getStartState :: U.Grid Int -> (U.Coord, U.Coord, U.Grid Int)
getStartState grid =
  let start =
        fst
          . head
          . filter (\(c, l) -> l == ord 'S')
          $ A.assocs grid
      end =
        fst
          . head
          . filter (\(c, l) -> l == ord 'E')
          $ A.assocs grid
      grid' = grid A.// [(start, ord 'a'), (end, ord 'z')]
   in (start, end, grid')

partOne str =
  let (start, end, grid) = getStartState $ toArray str
      fEnd coord = coord == start
   in fst
        . findMinPathDijkstra
          end
          fEnd
          (fNeighbours grid)
        $ fCost

partTwo str =
  let (_, end, grid) = getStartState $ toArray str
      fEnd coord = grid A.! coord == ord 'a'
   in fst
        . findMinPathDijkstra
          end
          fEnd
          (fNeighbours grid)
        $ fCost

main = do
  input <- readFile $ U.get2022Resources 12
  putStrLn "Part One:"
  print $ partOne example
  print $ partOne input
  putStrLn "Part Two:"

  print $ partTwo example
  print $ partTwo input
