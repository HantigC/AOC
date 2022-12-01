module Day15 where

import qualified Data.Array.IArray             as A
import           Data.Char                      ( digitToInt )
import           Data.List                      ( intercalate
                                                , nub
                                                , transpose
                                                )
import qualified Data.Map                      as M
import           Data.Maybe                     ( isNothing )
import           Data.MemoTrie                  ( memo )
import qualified Data.PQueue.Prio.Min          as PQ
import           Text.ParserCombinators.Parsec
import qualified Utils                         as U

cavernExample = intercalate
  "\n"
  [ "1163751742"
  , "1381373672"
  , "2136511328"
  , "3694931569"
  , "7463417111"
  , "1319128137"
  , "1359912421"
  , "3125421639"
  , "1293138521"
  , "2311944581\n"
  ]

cavernParser :: GenParser Char st [[Int]]
cavernParser = do
  mapp <- many (many digit U.==> char '\n')
  return $ map (map digitToInt) mapp

toArray :: [[Int]] -> A.Array (Int, Int) Int
toArray mapp =
  let h = length mapp
      w = length $ head mapp
  in  A.listArray ((0, 0), (h - 1, w - 1)) (concat mapp)

-- duplicateTilesWith :: [[Int]] -> (Int -> Int) -> Int -> Int -> [[Int]]
duplicateTilesWith x w = concat . f xs
 where
  xs = map concat . transpose . f x $ w
  f xx n = scanl
    (\acc _ ->
      map (map (\x -> (if (x + 1) `mod` 10 == 0 then 1 else x + 1))) acc
    )
    xx
    [2 .. n]

findLowestRiskPath :: U.Grid Int -> Int
findLowestRiskPath grid = f (h, w)
 where
  (_, (h, w)) = A.bounds grid
  total       = sum (A.elems grid) + 1
  fm          = memo f
  f (0, 0) = 0
  f (y, x) | y < 0 = total
           | x < 0 = total
           | y > h = total
           | x > w = total
  f (y, x) = grid A.! (y, x) + minimum [fm (y - 1, x), fm (y, x - 1)]

data CellCost = CellCost U.Coord Int
  deriving (Eq, Show)

instance Ord CellCost where
  (CellCost _ c1) < (CellCost _ c2) = c1 < c2
  (CellCost _ c1) <= (CellCost _ c2) = c1 <= c2

updatePQ :: (Ord k, Ord a) => [(k, a)] -> PQ.MinPQueue k a -> PQ.MinPQueue k a
updatePQ nodes pq = PQ.filterWithKey f pq `PQ.union` PQ.fromList nodes
 where
  ns = M.fromList $ map (\(x, y) -> (y, x)) nodes
  f cost coord = case M.lookup coord ns of
    Just c  -> cost < c
    Nothing -> True

graphMinDistance :: U.Grid Int -> Maybe Int
graphMinDistance grid = f (PQ.singleton 0 (0, 0)) M.empty
 where
  (_, fin) = A.bounds grid
  f priorityQ visited = if coord == fin
    then return cost
    else f (updatePQ blah q) (M.insert coord True visited)
   where
    ((cost, coord), q) = PQ.deleteFindMin priorityQ
    neighbs =
      filter (\x -> isNothing (M.lookup x visited))
        . U.getCrossNeighbCoords coord
        $ grid
    costs = [ grid A.! c | c <- neighbs ]
    blah  = zipWith (\c cc -> (c + cost, cc)) costs neighbs

partOne str = do
  grid <- parse cavernParser "" str
  return $ graphMinDistance $ toArray grid

partTwo str = do
  grid <- parse cavernParser "" str
  return $ graphMinDistance $ toArray $ duplicateTilesWith grid 5 5

main = do
  input <- readFile $ U.get2021Resources 15
  print $ partOne cavernExample
  print $ partOne input

  print $ partTwo cavernExample
  print $ partTwo cavernExample
