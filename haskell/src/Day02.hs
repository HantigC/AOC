module DayTwo where
import Data.List.Split as S

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

makeCommands :: [String] -> [(String, Int)]
makeCommands = map $ (\(x:y:_) -> (x, read y)) . S.splitOn " "

partOne :: [(String, Int)] -> Int
partOne xs = h * d
  where
    move (h, d) ("forward", x) = (h+x, d)
    move (h, d) ("down", x)    = (h, d+x)
    move (h, d) ("up", x)      = (h, d-x)
    move (_, _) (_, _)         = error "This should not happen"
    (h, d) = foldl move (0, 0) xs


partTwo :: [(String, Int)] -> Int
partTwo xs = h * d
  where
    move (h, d, a) ("forward", x) = (h+x, d + a*x, a)
    move (h, d, a) ("down", x)    = (h, d, a + x)
    move (h, d, a) ("up", x)      = (h, d, a - x)
    move (_, _, _) (_, _)         = error "This should not happen"
    (h, d, _) = foldl move (0, 0, 0) xs

main = do
  lines <- readLines "../resources/day_2.txt"
  print . partOne . makeCommands $ lines
  print . partTwo . makeCommands $ lines
