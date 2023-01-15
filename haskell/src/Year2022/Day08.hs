module Day08 where

import qualified Data.Array as A
import Data.Char (digitToInt)
import Data.List (intercalate)
import Data.MemoTrie
import Debug.Trace (trace)
import Text.ParserCombinators.Parsec
import qualified Utils as U

bazinLineToArray :: String -> A.Array (Int, Int) Int
bazinLineToArray xss = A.array idx elements
  where
    strs = lines xss
    idx = ((0, 0), (length strs - 1, length (head strs) - 1))
    elements =
      [ ((i, j), digitToInt x)
        | (i, xs) <- U.enumerate strs,
          (j, x) <- U.enumerate xs
      ]

type HeightMap a = A.Array (Int, Int) a

fromLeftUp :: HeightMap Int -> [((Int, Int), Bool)]
fromLeftUp heightMap =
  [((x, y), isVisible (x, y)) | x <- [w0 .. w], y <- [h0 .. h]]
  where
    heightBounds@((h0, w0), (h, w)) = A.bounds heightMap

    isVisible xy@(x, y)
      | xy `U.inBounds` ((1, 1), (h - 1, w - 1)) =
        any
          (heightAt xy >)
          [ f (x - 1, y) (\(x, y) -> (x - 1, y)) (\(x, y) -> x == 0),
            f (x + 1, y) (\(x, y) -> (x + 1, y)) (\(x, y) -> x == h),
            f (x, y - 1) (\(x, y) -> (x, y - 1)) (\(x, y) -> y == 0),
            f (x, y + 1) (\(x, y) -> (x, y + 1)) (\(x, y) -> y == w)
          ]
      | otherwise = True

    f xy fn fe
      | fe xy = heightAt xy
      | otherwise = max (heightAt xy) $ f (fn xy) fn fe

    heightAt xy = heightMap A.! xy

localHighest :: U.Coord -> HeightMap Int -> [Int]
localHighest xy@(x, y) heightMap = [fDown'', fLeft'', fUp'', fRight'']
  where
    mapBounds@((_, _), (height, width)) = A.bounds heightMap

    fRight'' =
      let h' = (fRight' xy $ heightMap A.! xy)
       in if y + h' >= width then h' else 1 + h'

    fLeft'' =
      let h' = (fLeft' xy $ heightMap A.! xy)
       in if y - h' <= 0 then h' else 1 + h'
    fDown'' =
      let h' = (fDown' xy $ heightMap A.! xy)
       in if x - h' <= 0 then h' else 1 + h'

    fUp'' =
      let h' = (fUp' xy $ heightMap A.! xy)
       in if x + h' >= height then h' else 1 + h'

    fRight' = memo fRight
    fRight xy@(x, y) h
      | y >= width = 0
      | h <= nextH = 0
      | otherwise = 1 + cnt + fRight' (x, y + cnt + 1) h
      where
        nextH = heightMap A.! (x, y + 1)
        cnt = fRight' (x, y + 1) h

    fLeft' = memo fLeft
    fLeft xy@(x, y) h
      | y <= 0 = 0
      | h <= nextH = 0
      | otherwise = 1 + cnt + fLeft' (x, y - cnt - 1) h
      where
        nextH = heightMap A.! (x, y - 1)
        cnt = fLeft' (x, y - 1) h

    fUp' = memo fUp
    fUp xy@(x, y) h
      | x >= height = 0
      | h <= nextH = 0
      | otherwise = 1 + cnt + fUp' (x + cnt + 1, y) h
      where
        nextH = heightMap A.! (x + 1, y)
        cnt = fUp' (x + 1, y) h

    fDown' = memo fDown
    fDown xy@(x, y) h
      | x <= 0 = 0
      | h <= nextH = 0
      | otherwise = 1 + cnt + fDown' (x - cnt - 1, y) h
      where
        nextH = heightMap A.! (x - 1, y)
        cnt = fDown' (x - 1, y) h

treeScore heightMap =
  [((x, y), localHighest (x, y) heightMap) | x <- [w0 .. w], y <- [h0 .. h]]
  where
    heightBounds@((h0, w0), (h, w)) = A.bounds heightMap

example = intercalate "\n" ["30373", "25512", "65332", "33549", "35390"]

partOne :: A.Array (Int, Int) Int -> Int
partOne = length . filter (== True) . map snd . fromLeftUp

partTwo = maximum . map (product . snd) . treeScore

main :: IO ()
main = do
  input <- readFile $ U.get2022Resources 8
  print $ partOne $ bazinLineToArray example
  print $ partOne $ bazinLineToArray input
  print $ partTwo $ bazinLineToArray example
  print $ partTwo $ bazinLineToArray input
