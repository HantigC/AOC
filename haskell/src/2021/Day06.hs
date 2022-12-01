module Day06 where
import qualified Data.List                     as L
import qualified Data.List.Split               as S
import qualified Data.Map                      as M
import qualified Utils                         as U


cntJelly :: [Int] -> [Int]
cntJelly ints = map (\x -> M.findWithDefault 0 x $ U.count ints) [0 .. 8]

grow :: [Int] -> Int -> [Int]
grow state 0 = state
grow [s0, s1, s2, s3, s4, s5, s6, s7, s8] no =
  grow [s1, s2, s3, s4, s5, s6, s0 + s7, s8, s0] $ no - 1
grow _ _ = error "This should not happen"


initialState = ["3,4,3,1,2"]


main :: IO ()
main = do
  initial <- U.readLines $ U.get2021Resources 6
  print $ sum . flip grow 256 . cntJelly . U.getIntList $ initial
