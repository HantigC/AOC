import Utils as U
import qualified Data.Map as M
import Data.Char (digitToInt, isLower, isUpper)
import qualified Data.List.Split as S
import Control.Monad (mapM, foldM)
import qualified Data.Array.IArray as A
import Data.List (sort)


graph = [ "start-A"
        , "start-b"
        , "A-c"
        , "A-b"
        , "b-d"
        , "A-end"
        , "b-end"
        ]
graph2 = [ "dc-end"
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

graph3 = [ "fs-end"
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
  where (fromNone:toNode:_) = S.splitOn "-" str


makeGraph :: [String] -> M.Map String [String]
makeGraph = foldl insertBothWays M.empty . map parseEdge
  where insertBothWays m (fromNode, toNode) = M.insertWith (++) toNode [fromNode] m'
          where m' = M.insertWith (++) fromNode [toNode] m

dfs  :: String
     -> M.Map String [String]
     -> [String]
     -> [[String]]
dfs [] _  visitedMap = []
dfs node graph visitedMap | node == "end" = [["end"]]
dfs node graph visitedMap
  | all isLower node =  doRecv  neighburNodes (node:visitedMap)
  | otherwise = doRecv  neighburNodes visitedMap
  where neighburNodes = filter (not . (`elem` visitedMap)) $ graph M.! node
        doRecv [] v = []
        doRecv (x:xs) visitedMap = map (node:) xss ++ paths
          where xss = dfs x graph visitedMap
                paths = doRecv xs visitedMap


partOne :: [String] -> Int
partOne strs = length $  dfs "start" (makeGraph strs) []


main :: IO ()
main = do
  lines <- U.readLines "resources/day_12.txt"
  print $ partOne graph
  print $ partOne graph2
  print $ partOne graph3
  print $ partOne lines

