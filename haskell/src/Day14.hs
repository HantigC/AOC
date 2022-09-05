module Day14 where

import Text.ParserCombinators.Parsec
import Data.List (nub, intercalate)
import qualified Data.Map as M

import qualified Utils as U



formulaExample = intercalate "\n" [ "NNCB"
                                   , ""
                                   , "CH -> B"
                                   , "HH -> N"
                                   , "CB -> H"
                                   , "NH -> C"
                                   , "HB -> C"
                                   , "HC -> B"
                                   , "HN -> C"
                                   , "NN -> C"
                                   , "BH -> H"
                                   , "NC -> B"
                                   , "NB -> B"
                                   , "BN -> B"
                                   , "BB -> N"
                                   , "BC -> B"
                                   , "CC -> N"
                                   , "CN -> C\n"
                                   ]


parsePolymerTemplate :: GenParser Char st String
parsePolymerTemplate = do many alphaNum


parsePairInsertion :: GenParser Char st (String, Char)
parsePairInsertion = do
  templateIn <- many alphaNum
  string " -> "
  templateOut <- alphaNum
  return (templateIn, templateOut)

parsePolymerization :: GenParser Char st (M.Map String Char, String)
parsePolymerization = do
  polymerTemplate <- parsePolymerTemplate U.==> char '\n'
  char '\n'
  inOuts <- many (parsePairInsertion U.==> char '\n')
  return (M.fromList inOuts, polymerTemplate)


polymerization :: String -> M.Map String Char -> String
polymerization [] _ = []
polymerization [x] _ = [x]
polymerization (x1:x2:xs) expentionMap = x1 : expansion
  where expansion = case M.lookup [x1, x2] expentionMap of
                      Just x -> x : polymerization (x2:xs) expentionMap
                      Nothing -> polymerization (x2:xs) expentionMap


polymerizations :: String -> M.Map String Char -> Int -> String
polymerizations str epansionMap n = foldl (\acc _ -> polymerization acc epansionMap) str [1..n]

countElements :: Ord a => [a] -> M.Map a Int
countElements = foldl f M.empty
  where f mapp chr = M.insertWith (+) chr 1 mapp



-- partOne :: String -> Either ParseError String
partOne str n = do
  (expansionMap, polymerTemplate) <- parse parsePolymerization "" str
  let cnts = M.elems $ countElements $ polymerizations polymerTemplate expansionMap n
      minCnt = minimum cnts
      maxCnt = maximum cnts
  return $ maxCnt - minCnt

main = do
  input <- readFile "../resources/Day14.txt"
  print $ partOne formulaExample 10
  print $ partOne input 10


  print $ partOne formulaExample 40
  print $ partOne input 40
