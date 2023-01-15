module Year2021.Day09 where

import qualified Data.Array as A
import Data.Bifunctor (bimap)
import Data.Char (digitToInt)
import Data.List (sort)
import qualified Utils as U

mapp = ["2199943210", "3987894921", "9856789892", "8767896789", "9899965678"]

bazinLineToArray :: [String] -> A.Array (Int, Int) Int
bazinLineToArray xss = A.array idx elements
  where
    idx = ((0, 0), (length xss - 1, length (head xss) - 1))
    elements =
      [ ((i, j), digitToInt x)
        | (i, xs) <- U.enumerate xss,
          (j, x) <- U.enumerate xs
      ]

findSmokeBasin :: A.Array (Int, Int) Int -> Maybe [(Int, Int)]
findSmokeBasin arr =
  sequence $
    filter (/= Nothing) [f (y, x) | y <- [0 .. h], x <- [0 .. w]]
  where
    bounds@(_, (h, w)) = A.bounds arr
    getIfInBounds coord
      | coord `U.inBounds` bounds = Just $ arr A.! coord
      | otherwise = Nothing

    f (y, x) = if all (> center) neighbours then Just (y, x) else Nothing
      where
        up = getIfInBounds (y - 1, x)
        down = getIfInBounds (y + 1, x)
        left = getIfInBounds (y, x - 1)
        right = getIfInBounds (y, x + 1)
        neighbours = filter (/= Nothing) [down, up, left, right]
        center = getIfInBounds (y, x)

computeNeighbCoords :: (Int, Int) -> [(Int, Int)]
computeNeighbCoords (y, x) =
  map (bimap (+ y) (+ x)) [(-1, 0), (0, 1), (1, 0), (0, -1)]

getNeighbours :: (Int, Int) -> A.Array (Int, Int) a -> [(Int, Int)]
getNeighbours coord@(y, x) arr =
  filter (`U.inBounds` bounds) . computeNeighbCoords $ coord
  where
    bounds = A.bounds arr

extractBasins ::
  [(Int, Int)] ->
  A.Array (Int, Int) Int ->
  A.Array (Int, Int) Bool ->
  [[(Int, Int)]]
extractBasins [] _ _ = []
extractBasins (s : ss) mapp visitedMask =
  basinSize : extractBasins ss mapp visitedMask'
  where
    (basinSize, visitedMask') = extractBasin [s] (visitedMask A.// [(s, True)])
    extractBasin [] visitedMask = ([], visitedMask)
    extractBasin (x : xs) visitedMask = (x : num, visitedMask)
      where
        neighCoords = getNeighbours x mapp
        currValue = mapp A.! x
        notVisited = map not $ U.atIxds visitedMask neighCoords
        neighValues = U.atIxds mapp neighCoords
        inBasinValues = map (\v -> v < 9 && v > currValue) neighValues
        ok = zipWith (&&) inBasinValues notVisited
        oks = filter snd (zip neighCoords ok)
        newCoords = map fst oks
        (num, visitedMask') =
          extractBasin
            (xs ++ newCoords)
            (visitedMask A.// zip newCoords (repeat True))

partOne' :: [String] -> Maybe Int
partOne' strBasins = do
  basins <- smokeBasins
  return $ sum . map (+ 1) . U.atIxds mapp $ basins
  where
    mapp = bazinLineToArray strBasins
    smokeBasins = findSmokeBasin mapp

partTwo :: [String] -> Maybe Int
partTwo strBasins = do
  bazins <- smokeBasins
  return $
    product . take 3 . reverse . sort . map length $
      extractBasins
        bazins
        mapp
        visitedMask
  where
    mapp = bazinLineToArray strBasins
    ((h0, w0), (h, w)) = A.bounds mapp
    visitedMask =
      A.listArray (A.bounds mapp) $ replicate ((w - w0 + 1) * (h - h0 + 1)) False
    smokeBasins = findSmokeBasin mapp

main :: IO ()
main = do
  lines <- U.readLines $ U.get2021Resources 9
  print $ partOne' mapp
  print $ partOne' lines
  print $ partTwo mapp
  print $ partTwo lines
