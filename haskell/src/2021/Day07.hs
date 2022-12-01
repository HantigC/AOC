module Day07 where
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import qualified Utils                         as U


computeCost :: (Int -> Int -> Int -> Int) -> Map Int Int -> Int
computeCost costFn mp = minimum costs
 where
  minV  = fst $ M.findMin mp
  maxV  = fst $ M.findMax mp
  costs = map (\dest -> M.foldlWithKey (go dest) 0 mp) [minV .. maxV]
  go dest acc src amount = (+) acc $ costFn src dest amount


sumLoss :: Int -> Int -> Int -> Int
sumLoss src dest amount = (*) amount $ abs $ src - dest


arithmLoss :: Int -> Int -> Int -> Int
arithmLoss src dest amount = (*) amount $ go . abs $ src - dest
  where go n = flip div 2 $ n * (n + 1)


mapMulTwo :: [Int] -> [Int]
mapMulTwo = map (* 2)


initialState = ["16,1,2,0,4,2,7,1,2,14"]

main :: IO ()
main = do
  s <- U.readLines $ U.get2021Resources 7
  print $ computeCost sumLoss . U.count . U.getIntList $ s
  print $ computeCost arithmLoss . U.count . U.getIntList $ s
