module PathFinding where

import qualified Data.Array.IArray as A
import qualified Data.Map as M
import Data.Maybe (isNothing)
import qualified Data.PQueue.Prio.Min as PQ
import Data.Tuple (swap)
import qualified Utils as U

updatePQ :: (Ord k, Ord a) => [(k, a)] -> PQ.MinPQueue k a -> PQ.MinPQueue k a
updatePQ nodes pq = PQ.filterWithKey f pq `PQ.union` PQ.fromList nodes
  where
    ns = M.fromList $ map swap nodes
    f cost coord = case M.lookup coord ns of
      Just c -> cost < c
      Nothing -> True

findMinPathDijkstra ::
  Ord a =>
  a ->
  (a -> Bool) ->
  (a -> [a]) ->
  (a -> Int) ->
  (Int, [a])
findMinPathDijkstra start fEnd fNeighbours fCost =
  go
    (PQ.singleton 0 start)
    M.empty
    $ M.singleton start start
  where
    reconstructPath to path = f to []
      where
        f node acc
          | prevNode == node = acc
          | otherwise = f prevNode (node : acc)
          where
            prevNode = path M.! node

    go priorityQ visited path =
      if fEnd nextItem
        then (cost, reconstructPath nextItem path)
        else
          go
            (updatePQ blah q)
            (M.insert nextItem True visited)
            $ foldl updatePath path (zip neighbs $ repeat nextItem)
      where
        ((cost, nextItem), q) = PQ.deleteFindMin priorityQ
        updatePath path' (to, from) = M.insert to from path'
        neighbs =
          filter (\x -> isNothing (M.lookup x visited))
            . fNeighbours
            $ nextItem
        costs = [fCost x | x <- neighbs]

        blah = zipWith (\c cc -> (c + cost, cc)) costs neighbs
