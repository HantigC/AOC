module Day01 where
import           Data.List                      ( sort
                                                , sortOn
                                                )
import           Data.List.Split                ( splitOn )
import qualified Utils                         as U

toCalories :: [String] -> [[Int]]
toCalories lines = map (map read) $ splitOn [""] lines

toCaloriesPerElf :: [String] -> [Int]
toCaloriesPerElf = map sum . toCalories

partOne :: [String] -> Int
partOne = maximum . toCaloriesPerElf

partTwo :: [String] -> Int
partTwo = sum . take 3 . sortOn (\x -> -x) . toCaloriesPerElf

main :: IO ()
main = do
    lines <- U.readLines $ U.get2022Resources 1
    print $ partOne lines
    print $ partTwo lines
