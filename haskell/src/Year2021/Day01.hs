module Year2021.Day01 where

import           Utils
import qualified Utils                         as U


conv :: Int -> [Int] -> Int
conv num xn = snd $ foldl cumConv (take num xn, 0) $ drop num xn
 where
  cumConv ([], _) _ = ([], 0)
  cumConv (pp@(p : pl), cnt) cur
    | sum pp < sum pl + cur = (pl ++ [cur], cnt + 1)
    | otherwise             = (pl ++ [cur], cnt)



main = do
  lines <- Utils.readLines $ U.get2021Resources 1
  print "here"
  print $ conv 1 $ map read lines
  print $ conv 3 $ map read lines

