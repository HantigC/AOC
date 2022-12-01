module Day12 where
import           Control.Monad                  ( foldM
                                                , mapM
                                                )
import qualified Data.Array.IArray             as A
import           Data.Char                      ( digitToInt
                                                , isLower
                                                , isUpper
                                                )
import           Data.List                      ( sort )
import qualified Data.List.Split               as S
import qualified Data.Map                      as M
import qualified Data.Set                      as ST
import           Utils                         as U


graph = ["start-A", "start-b", "A-c", "A-b", "b-d", "A-end", "b-end"]
graph2 =
  [ "dc-end"
  , "HN-start"
  , "start-kj"
  , "dc-start"
  , "dc-HN"
  , "LN-dc"
  , "HN-end"
  , "kj-sa"
  , "kj-HN"
  , "kj-dc"
  ]

graph3 =
  [ "fs-end"
  , "he-DX"
  , "fs-he"
  , "start-DX"
  , "pj-DX"
  , "end-zg"
  , "zg-sl"
  , "zg-pj"
  , "pj-he"
  , "RW-he"
  , "fs-DX"
  , "pj-RW"
  , "zg-RW"
  , "start-pj"
  , "he-WI"
  , "zg-he"
  , "pj-fs"
  , "start-RW"
  ]


type Edge = (String, String)

data NodeType = Visited
              | Neighbours [String]

parseEdge :: String -> Edge
parseEdge str = (fromNone, toNode)
  where (fromNone : toNode : _) = S.splitOn "-" str


makeGraph :: [String] -> M.Map String [String]
makeGraph = foldl insertBothWays M.empty . map parseEdge
 where
  insertBothWays m (fromNode, toNode) = M.insertWith (++) toNode [fromNode] m'
    where m' = M.insertWith (++) fromNode [toNode] m


dfs :: String -> M.Map String [String] -> [String] -> [[String]]
dfs [] _ visitedMap                       = []
dfs node graph visitedMap | node == "end" = [["end"]]
dfs node graph visitedMap
  | all isLower node = doRecv neighburNodes (node : visitedMap)
  | otherwise        = doRecv neighburNodes visitedMap
 where
  neighburNodes = filter (not . flip elem visitedMap) $ graph M.! node
  doRecv []       v          = []
  doRecv (x : xs) visitedMap = map (node :) xss ++ paths
   where
    xss   = dfs x graph visitedMap
    paths = doRecv xs visitedMap


dfs' :: String -> M.Map String [String] -> Bool -> [String] -> [[String]]
dfs' [] _ was visitedMap                     = []
dfs' node graph _ visitedMap | node == "end" = [["end"]]
dfs' node graph was visitedMap
  | node == "start"
  = doRecv neighburNodes (node : visitedMap) False
  | all isLower node && was
  = doRecv neighburNodes (node : visitedMap) was
  | all isLower node && not was
  = doRecv neighburNodes visitedMap True
    ++ doRecv neighburNodes (node : visitedMap) False
  | otherwise
  = doRecv neighburNodes visitedMap was
 where
  neighburNodes = filter (not . flip elem visitedMap) $ graph M.! node
  doRecv []       v          _   = []
  doRecv (x : xs) visitedMap was = map (node :) xss ++ paths
   where
    xss   = dfs' x graph was visitedMap
    paths = doRecv xs visitedMap was

partOne :: [String] -> Int
partOne strs = length $ dfs "start" (makeGraph strs) []

partTwo strs = length $ ST.fromList $ dfs' "start" (makeGraph strs) False []

main :: IO ()

main = do
  lines <- U.readLines $ U.get2021Resources 12
  print $ partOne graph
  print $ partOne graph2
  print $ partOne graph3
  print $ partOne lines

  print $ partTwo graph
  print $ partTwo graph2
  print $ partTwo graph3
  print $ partTwo lines
