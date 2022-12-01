module Day08 where

import           Control.Monad                  ( mapM )
import qualified Control.Monad                 as CM
import           Data.List                      ( sort )
import qualified Data.List.Split               as S
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import qualified Data.Set                      as Set
import qualified Utils                         as U



get4Nums :: [String] -> [[String]]
get4Nums = map (words . (!! 1) . S.splitOn " | ")

partOne :: [String] -> Int
partOne lines = sum . map (\k -> M.findWithDefault 0 k mm) $ [2, 3, 4, 7]
  where mm = U.count . map length . concat . get4Nums $ lines


mapps = M.fromList
  [ (sort "acedgfb", 8)
  , (sort "cdfbe"  , 5)
  , (sort "gcdfa"  , 2)
  , (sort "fbcad"  , 3)
  , (sort "dab"    , 7)
  , (sort "cefabd" , 9)
  , (sort "cdfgeb" , 6)
  , (sort "eafb"   , 4)
  , (sort "cagedb" , 0)
  , (sort "ab"     , 1)
  ]



bitToDec :: [Int] -> Int
bitToDec = fst . foldr (\x (num, cnt) -> (num + x * 2 ^ cnt, cnt + 1)) (0, 0)


example =
  [ "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
  , "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
  , "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
  , "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
  , "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
  , "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
  , "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
  , "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
  , "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
  , "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
  , "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
  ]
posibleNums =
  [ --Set.fromList "be"
              -- Set.fromList "cfbegad"
    Set.fromList "cbdgef"
  , Set.fromList "fgaecd"
              -- Set.fromList "cgeb"
  , Set.fromList "fdcge"
  , Set.fromList "agebfd"
  , Set.fromList "fecdb"
  , Set.fromList "fabcd"
              -- Set.fromList "edb"
  ]

findNumbers :: [Set.Set Char] -> Maybe (M.Map (Set.Set Char) Char)
findNumbers sevenSegments = do
  let one         = getSingle 2
  let four        = getSingle 4
  let seven       = getSingle 3
  let eight       = getSingle 7

  let fiveLengths = getByLengths 5
  let sixLengths  = getByLengths 6

  (three, fiveLengths) <- oneAndRest (sizeEqTo 2 . (one `Set.intersection`))
                                     fiveLengths
  let zeroPattern = eight `Set.difference` three `Set.union` one

  (zero, sixLengths) <- oneAndRest (sizeEqTo 4 . Set.intersection zeroPattern)
                                   sixLengths
  (nine, sixLengths) <- oneAndRest (sizeEqTo 2 . Set.intersection one)
                                   sixLengths

  (five, fiveLengths) <- oneAndRest
    (sizeEqTo 6 . Set.intersection nine . Set.union one)
    fiveLengths

  let six = head sixLengths
  let two = head fiveLengths
  Just $ M.fromList
    [ (zero , '0')
    , (one  , '1')
    , (two  , '2')
    , (three, '3')
    , (four , '4')
    , (five , '5')
    , (six  , '6')
    , (seven, '7')
    , (eight, '8')
    , (nine , '9')
    ]

 where
  getByLengths x = filter ((== x) . Set.size) sevenSegments
  getSingle = head . getByLengths
  sizeEqTo x = (== x) . Set.size


oneAndRest :: (a -> Bool) -> [a] -> Maybe (a, [a])
oneAndRest f [] = Nothing
oneAndRest f (x : xs)
  | f x = Just (x, xs)
  | otherwise = do
    (y, ys) <- oneAndRest f xs
    Just (y, x : ys)


partTwo ls = do
  ints <- mapM f ls
  Just $ sum ints
 where
  f x = do
    patternMap <- findNumbers $ map (Set.fromList . sort) searchSpace
    stringInt <- mapM ((`M.lookup` patternMap) . Set.fromList . sort) matchSpace
    Just (read stringInt :: Int)
    where (searchSpace : matchSpace : _) = map words . S.splitOn " | " $ x


main :: IO ()
main = do
  lines <- U.readLines $ U.get2021Resources 8
  print . partOne $ lines
  print . partTwo $ lines
