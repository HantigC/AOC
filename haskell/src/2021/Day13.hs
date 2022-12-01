module Day13 where

import           Data.List                      ( intercalate
                                                , nub
                                                )
import           Text.ParserCombinators.Parsec
import qualified Utils                         as U

data Fold = FoldY Int
          | FoldX Int
          deriving (Show, Eq)


paper = intercalate
  "\n"
  [ "6,10"
  , "0,14"
  , "9,10"
  , "0,3"
  , "10,4"
  , "4,11"
  , "6,0"
  , "6,12"
  , "4,1"
  , "0,13"
  , "10,12"
  , "3,4"
  , "3,0"
  , "8,4"
  , "1,10"
  , "2,14"
  , "8,10"
  , "9,0"
  , ""
  , "fold along x=5"
  , "fold along y=7\n"
  ]


data TransparentPaper = TransparentPaper
  { paperStensil :: [(Int, Int)]
  , folds        :: [Fold]
  }



parseCoords :: GenParser Char st (Int, Int)
parseCoords = do
  x <- many digit
  char ','
  y <- many digit
  return (read x, read y)


parseFold :: GenParser Char st Fold
parseFold = do
  string "fold along "
  foldType   <- parseX <|> parseY
  coordinate <- many digit
  return $ foldType (read coordinate)
 where
  parseX = string "x=" >> return FoldX
  parseY = string "y=" >> return FoldY




parseTransparentPaper :: GenParser Char st TransparentPaper
parseTransparentPaper = do
  coordinates <- many (parseCoords U.==> char '\n')
  char '\n'
  folds <- many (parseFold U.==> char '\n')
  return (TransparentPaper coordinates folds)


showPaper :: [(Int, Int)] -> String
showPaper coords = intercalate
  "\n"
  [ [ f x y | x <- [0 .. maxX] ] | y <- [0 .. maxY] ]
 where
  maxX = maximum . map fst $ coords
  maxY = maximum . map snd $ coords
  f y x = if (y, x) `elem` coords then '#' else '.'


foldTransparentPaper :: [(Int, Int)] -> Fold -> [(Int, Int)]
foldTransparentPaper coords foldType = case foldType of
  (FoldX foldX) ->
    nub
      . map (\(x, y) -> (f x foldX maxX, y))
      . filter ((/= foldX) . fst)
      $ coords
  (FoldY foldY) ->
    nub
      . map (\(x, y) -> (x, f y foldY maxY))
      . filter ((/= foldY) . snd)
      $ coords
 where
  f coord foldCoord maxCoord | coord < foldCoord = coord
                             | otherwise         = maxCoord - coord
  maxX = maximum . map fst $ coords
  maxY = maximum . map snd $ coords


partOne :: Either ParseError TransparentPaper -> Either ParseError Int
partOne transparentPaper = do
  (TransparentPaper paper folds) <- transparentPaper
  return $ length . foldl foldTransparentPaper paper $ [head folds]


partTwo :: Either ParseError TransparentPaper -> String
partTwo transparentPaper = case transparentPaper of
  (Right (TransparentPaper paper folds)) ->
    showPaper $ foldl foldTransparentPaper paper folds
  (Left _) -> ""


main = do
  resource <- readFile $ U.get2021Resources 13
  print $ partOne $ parse parseTransparentPaper "" paper
  print $ partOne $ parse parseTransparentPaper "" resource
  putStr $ partTwo $ parse parseTransparentPaper "" paper
  putStr $ partTwo $ parse parseTransparentPaper "" resource
