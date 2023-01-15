module Year2022.Day11 where

import Data.Either (fromRight)
import Data.List (intercalate, sort)
import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import Utils as U

example' =
  intercalate
    "\n"
    [ "Monkey 0:",
      "  Starting items: 79, 98",
      "  Operation: new = old * 19",
      "  Test: divisible by 23",
      "    If true: throw to monkey 2",
      "    If false: throw to monkey 3",
      "",
      "Monkey 1:",
      "  Starting items: 54, 65, 75, 74",
      "  Operation: new = old + 6",
      "  Test: divisible by 19",
      "    If true: throw to monkey 2",
      "    If false: throw to monkey 0",
      "",
      "Monkey 2:",
      "  Starting items: 79, 60, 97",
      "  Operation: new = old * old",
      "  Test: divisible by 13",
      "    If true: throw to monkey 1",
      "    If false: throw to monkey 3",
      "",
      "Monkey 3:",
      "  Starting items: 74",
      "  Operation: new = old + 3",
      "  Test: divisible by 17",
      "    If true: throw to monkey 0",
      "    If false: throw to monkey 1"
    ]

data Monkey = Monkey
  { mId :: Integer,
    stratingItems :: [Integer],
    operation :: Operation,
    test :: Test,
    inpection :: Int
  }
  deriving (Show)

data Operand = Val Integer | Var deriving (Show)

data Operator = Mul | Div | Add | Sub deriving (Show)

data Operation = Operation Operand Operator Operand deriving (Show)

data Test = Test
  {divisible :: Integer, tIfTrue :: Integer, tIfFalse :: Integer}
  deriving (Show)

parseNumber :: GenParser Char st Integer
parseNumber = read <$> many digit

parseId = do
  string "Monkey "
  parseNumber U.==> char ':'

parseStartingItems = do
  skipMany (char ' ')
  string "Starting items: "
  parseNumber `sepBy` string ", "

parseOperator :: GenParser Char st Operator
parseOperator = do
  x <- anyChar
  return $ case x of
    '*' -> Mul
    '-' -> Sub
    '+' -> Add
    '/' -> Div
    _ -> error " should be +-/*"

parseOperand :: GenParser Char st Operand
parseOperand = do
  old <- string "old" <|> many digit
  return $ case old of
    "old" -> Var
    x -> Val (read x)

parseThrow how = do
  skipMany (char ' ')
  string "If "
  string how
  string ": throw to monkey "
  parseNumber

parseMTest = do
  skipMany (char ' ')
  string "Test: divisible by "
  divNumber <- parseNumber
  char '\n'
  ifTrue <- parseThrow "true"
  char '\n'
  ifFalse <- parseThrow "false"
  optional $ char '\n'
  return $ Test divNumber ifTrue ifFalse

parseOperation = do
  skipMany (char ' ')
  string "Operation: new = "
  stOperand <- parseOperand
  char ' '
  operator <- parseOperator
  char ' '
  Operation stOperand operator <$> parseOperand

parseMonkey = do
  id <- parseId
  char '\n'
  startingItems <- parseStartingItems
  char '\n'
  operation <- parseOperation
  char '\n'
  test <- parseMTest
  return $ Monkey id startingItems operation test 0

parseMonkeys = parseMonkey `sepBy` string "\n"

getValue x Var = x
getValue _ (Val x) = x

executeOp x (Operation op1 oper op2) =
  let op1' = getValue x op1
      op2' = getValue x op2
   in case oper of
        Mul -> op1' * op2'
        Div -> op1' `div` op2'
        Add -> op1' + op2'
        Sub -> op1' - op2'

monkeyInspect f (Monkey id items op t am) monkeys =
  foldl insertToMonkey monkeys' items'
  where
    monkeys' = M.insert id (Monkey id [] op t am') monkeys
    am' = am + length items
    allProd = product . map (divisible . test) . M.elems $ monkeys
    updateItem x = x `mod` allProd
    items' = [updateItem . f $ executeOp item op | item <- items]
    insertToMonkey monkeys item =
      let nextMId = executeTest item t
          (Monkey nId nItems nOp nT nAm) = monkeys M.! nextMId
       in M.insert nId (Monkey nId (nItems ++ [item]) nOp nT nAm) monkeys

monkeyRound f monkeys = foldl ff monkeys $ M.keys monkeys
  where
    ff monkeys id = monkeyInspect f (monkeys M.! id) monkeys

monkeyRounds ff monkeys num = foldl f monkeys [1 .. num]
  where
    f m _ = monkeyRound ff m

createMonkeyMap str =
  let monkeys = fromRight [] $ parse parseMonkeys "" str
   in M.fromList [(mId monkey, monkey) | monkey <- monkeys]

executeTest item (Test d t f)
  | item `mod` d == 0 = t
  | otherwise = f

monkeys' = createMonkeyMap example'

monkey' = monkeys' M.! 0

solve f rounds str =
  let monkeys = createMonkeyMap str
      roundedMonkeys = monkeyRounds f monkeys rounds
   in product
        . take 2
        . sort
        . map ((\x -> (- x)) . inpection)
        . M.elems
        $ roundedMonkeys

partOne = solve (`div` 3) 20

partTwo = solve id 10000

main = do
  input <- readFile $ U.get2022Resources 11
  print $ partOne example'
  print $ partOne input
  print $ partTwo example'
  print $ partTwo input
