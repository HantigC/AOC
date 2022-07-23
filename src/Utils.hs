module Utils where

import Data.Map (Map)
import qualified Data.Map as M
import Data.List.Split as S
import qualified Data.Array as A


type Coord = (Int, Int)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

count :: Ord a => [a] -> Map a Int
count = foldl (\m k -> M.insertWith (+) k 1 m) M.empty


getIntList :: [String] -> [Int]
getIntList =  foldl (\acc x -> (++) acc $ map read . S.splitOn "," $ x) []


atIxds :: A.Array (Int, Int) a -> [(Int, Int)] -> [a]
atIxds arr idxs = [arr A.! coord | coord <- idxs]

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]
