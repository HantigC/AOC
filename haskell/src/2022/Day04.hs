module Day04 where
import           Data.Either                    ( fromRight )
import           Data.List                      ( intercalate )
import           Text.ParserCombinators.Parsec
import           Utils                          ( (==>)
                                                , get2022Resources
                                                )


parsePair :: GenParser Char st ((Int, Int), (Int, Int))
parsePair = do
  stLeft <- many digit
  char '-'
  stRigt <- many digit
  char ','
  ndLeft <- many digit
  char '-'
  ndRigt <- many digit
  return ((read stLeft, read stRigt), (read ndLeft, read ndRigt))

parsePairs = many (parsePair ==> optional (char '\n'))

partOne = sum . map f
 where
  f ((xl, xr), (yl, yr)) | xl <= yl && yr <= xr = 1
                         | yl <= xl && xr <= yr = 1
                         | otherwise            = 0

partTwo = sum . map f
 where
  f ((xl, xr), (yl, yr)) | minR >= maxL = 1
                         | otherwise    = 0
   where
    maxL = max xl yl
    minR = min xr yr

example = intercalate
  "\n"
  ["2-4,6-8", "2-3,4-5", "5-7,7-9", "2-8,3-7", "6-6,4-6", "2-6,4-8"]

main = do
  input <- readFile $ get2022Resources 4
  let pairs        = fromRight [] $ parse parsePairs "" input
      examplePairs = fromRight [] $ parse parsePairs "" example
  print $ partOne examplePairs
  print $ partOne pairs

  print $ partTwo examplePairs
  print $ partTwo pairs
