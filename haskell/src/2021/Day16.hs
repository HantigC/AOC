module Day16 where

import qualified Data.Array.IArray             as A
import           Data.Char                      ( digitToInt )
import           Data.List                      ( intercalate
                                                , nub
                                                , transpose
                                                )
import qualified Data.Map                      as M
import           Data.Maybe                     ( isNothing )
import           Data.MemoTrie                  ( memo )
import qualified Data.PQueue.Prio.Min          as PQ
import           Text.ParserCombinators.Parsec
import qualified Utils                         as U

hexToBinMap = M.fromList
  [ ('0', "0000")
  , ('1', "0001")
  , ('2', "0010")
  , ('3', "0011")
  , ('4', "0100")
  , ('5', "0101")
  , ('6', "0110")
  , ('7', "0111")
  , ('8', "1000")
  , ('9', "1001")
  , ('A', "1010")
  , ('B', "1011")
  , ('C', "1100")
  , ('D', "1101")
  , ('E', "1110")
  , ('F', "1111")
  ]

data BITS
  = LiteralValue {version :: Int, value :: Int, packType :: Int}
  | Empty String Int
  | Operator {arity :: Int, version :: Int, subpackes :: [BITS], packType :: Int, lengthType :: Int}
  deriving (Show)

parsePackage :: GenParser Char st BITS
parsePackage = do
  packageVersion <- count 3 digit
  packageType    <- count 3 digit
  case packageType of
    "100" -> do
      litValue <- parseLiteral
      return $ LiteralValue (U.binToDecStr packageVersion) litValue 4
    packType -> do
      oneZero <- digit

      case oneZero of
        '0' -> do
          fifteenBits <- count 15 digit
          pos         <- sourceColumn <$> getPosition
          s           <- count (U.binToDecStr fifteenBits) digit

          let packages = parseWied s

          return $ Operator (length packages)
                            (U.binToDecStr packageVersion)
                            packages
                            (U.binToDecStr packType)
                            0
        _ -> do
          elevenBits <- count 11 digit
          packages   <- count (U.binToDecStr elevenBits) parsePackage
          return $ Operator (length packages)
                            (U.binToDecStr packageVersion)
                            packages
                            (U.binToDecStr packType)
                            1
 where
  parseNext n = do
    pos <- sourceColumn <$> getPosition
    if pos == n then try anyChar else try (char 'G')

parseWied str = case parse (many parsePackage) "" str of
  Left  _    -> [Empty str 0]
  Right bits -> bits

parseLiteral :: GenParser Char st Int
parseLiteral = U.binToDecStr <$> f
 where
  f = do
    oneZero <- digit
    case oneZero of
      '0' -> count 4 digit
      _   -> do
        fourBits <- count 4 digit
        bits     <- f
        return (fourBits ++ bits)

sumVersions :: BITS -> Int
sumVersions (LiteralValue version _ _) = version
sumVersions (Empty _ _               ) = 0
sumVersions (Operator _ version subpackes _ _) =
  version + (sum . map sumVersions $ subpackes)

compute :: BITS -> Maybe Int
compute (LiteralValue version value _) = Just value
compute (Operator _ _ subpackes 0 _  ) = sum <$> mapM compute subpackes
compute (Operator _ _ subpackes 1 _  ) = product <$> mapM compute subpackes
compute (Operator _ _ subpackes 2 _  ) = minimum <$> mapM compute subpackes
compute (Operator _ _ subpackes 3 _  ) = maximum <$> mapM compute subpackes
compute (Operator _ _ [p1, p2] 5 _) =
  boolToInt <$> ((>) <$> compute p1 <*> compute p2)
compute (Operator _ _ [p1, p2] 6 _) =
  boolToInt <$> ((<) <$> compute p1 <*> compute p2)
compute (Operator _ _ [p1, p2] 7 _) =
  boolToInt <$> ((==) <$> compute p1 <*> compute p2)
compute _ = Nothing

boolToInt False = 0
boolToInt True  = 1

parseHex = concatMap (hexToBinMap M.!) . takeWhile (/= '\n')

partOne str = do
  let bin = parseHex str
  bits <- parse parsePackage "" bin
  return $ sumVersions bits

partTwo str = do
  let bin = parseHex str
  bits <- parse parsePackage "" bin
  return $ compute bits

main = do
  input <- readFile $ U.get2021Resources 16
  print $ partOne input
  print $ partTwo input
