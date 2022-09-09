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

polymerization :: (M.Map String Int, M.Map Char Int)
                -> M.Map String Char
                -> (M.Map String Int, M.Map Char Int)
polymerization (polymerMap, charCnt) expansionMap = M.foldrWithKey f (M.empty, charCnt) polymerMap
  where f (x1:x2:_) n (mapp, cntMap)  = ( M.insertWith (+) [x1,xm] n (M.insertWith (+) [xm,x2] n mapp)
                                        , M.insertWith (+) xm n cntMap)
          where xm = expansionMap M.! [x1,x2]
        f _ _ _ = error "multiple elements in list"

polymerizations :: (M.Map String Int, M.Map Char Int)
                -> M.Map String Char
                -> Int
                -> (M.Map String Int, M.Map Char Int)
polymerizations polymerMap expansionMap n = foldl (\acc _ -> polymerization acc expansionMap) polymerMap [1..n]


countElements :: Ord a => [a] -> M.Map a Int
countElements = foldl f M.empty
  where f mapp chr = M.insertWith (+) chr 1 mapp

partOne :: String -> Int -> Either ParseError Int
partOne str n = do
  (expansionMap, polymerTemplate) <- parse parsePolymerization "" str
  let (s, cnts) = polymerizations (polymerMap, countElements polymerTemplate) expansionMap n
      polymerMap = M.fromListWith (+) $ zip (U.convolve polymerTemplate 2) (repeat 1)
      minCnt = minimum $ M.elems cnts
      maxCnt = maximum $ M.elems cnts
  return $ maxCnt - minCnt

main = do
  input <- readFile "../resources/Day14.txt"
  print $ partOne  formulaExample 10
  print $ partOne input 10

  print $ partOne formulaExample 40
  print $ partOne input 40
