module DayFour where

import qualified Data.List.Split as S
import qualified Utils as U


type BingoSeq = [Int]
type BingoBoard = [[(Int, Bool)]]
type BingoGame = (BingoSeq, [BingoBoard])
type Score = Int
type Winner = Maybe Score

toBingoSeq :: String -> BingoSeq
toBingoSeq = map read . S.splitOn ","

toBingoBoard :: [String] -> BingoBoard
toBingoBoard =
  map
  $ map (\x -> (read x, False))
  . filter (/= "")
  . S.splitOn " "

toBingoBoards :: [String] -> [BingoBoard]
toBingoBoards = makeBoard []
  where
    makeBoard [] [] = []
    makeBoard strs [] = [toBingoBoard strs]
    makeBoard [] ("":bs) = makeBoard [] bs
    makeBoard strs ("":bs) = toBingoBoard strs : makeBoard [] bs
    makeBoard strs (str:bs) = makeBoard (strs ++ [str]) bs

playBingo :: (Int -> [BingoBoard] -> ([BingoBoard], Maybe BingoBoard)) -> BingoGame -> Winner
playBingo strategy (seq, boards) = playNext seq boards
  where
    playNext [] boards = Nothing
    playNext (s:ss) boards =
      case strategy s boards of
        (updatedBoards, Nothing) -> playNext ss updatedBoards
        (_, Just board) -> Just $ (* s) $ sum . map (sum . map fst . filter (not . snd)) $ board

toBingoGame :: [String] -> BingoGame
toBingoGame []           = ([], [])
toBingoGame (seq:boards) = (toBingoSeq seq, toBingoBoards boards)

updateBoard :: Int -> BingoBoard -> BingoBoard
updateBoard num = map $ map (\cell@(num' , _) -> if num' == num then (num , True) else cell)

checkBoard :: BingoBoard -> Bool
checkBoard = doCheck []
  where
    doCheck [] bs@(b:_) = doCheck (replicate (length b) True) bs
    doCheck prev  []  = or prev
    doCheck prev (b:bs)
      | and checkedRow = True
      | otherwise = doCheck (zipWith (&&) prev checkedRow) bs
      where checkedRow = map snd b


winOnFirst :: Int -> [BingoBoard] -> ([BingoBoard], Maybe BingoBoard)
winOnFirst s boards = (updatedBoards, getFirstWinBoard updatedBoards)
  where
    updatedBoards = map (updateBoard s) boards
    getFirstWinBoard :: [BingoBoard] -> Maybe BingoBoard
    getFirstWinBoard [] = Nothing
    getFirstWinBoard (b:bs)
      | checkBoard b = Just b
      | otherwise = getFirstWinBoard bs


winOnLast :: Int -> [BingoBoard] -> ([BingoBoard], Maybe BingoBoard)
winOnLast s [board]
  | checkBoard updatedBoard = ([updatedBoard], Just updatedBoard)
  | otherwise = ([updatedBoard], Nothing)
  where updatedBoard = updateBoard s board
winOnLast s boards = (updatedBoards, Nothing)
  where
    updatedBoards = filter (not . checkBoard ) . map (updateBoard s) $ boards


boards = [ "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
         , ""
         , "22 13 17 11  0"
         , " 8  2 23  4 24"
         , "21  9 14 16  7"
         , " 6 10  3 18  5"
         , " 1 12 20 15 19"
         , ""
         , " 3 15  0  2 22"
         , " 9 18 13 17  5"
         , "19  8  7 25 23"
         , "20 11 10 24  4"
         , "14 21 16 12  6"
         , ""
         , "14 21 17 24  4"
         , "10 16 15  9 19"
         , "18  8 23 26 20"
         , "22 11 13  6  5"
         , " 2  0 12  3  7"]

main = do
  lines <- U.readLines "resources/day_4.txt"
  print $ playBingo winOnFirst $ toBingoGame lines
  print $ playBingo winOnLast $ toBingoGame lines
  -- putStrLn $ show $ toBingo $ lines
