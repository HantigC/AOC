module Day05 where

import           Data.Either                    ( fromRight )
import           Data.List                      ( intercalate
                                                , transpose
                                                )
import           Text.Parsec                    ( endOfLine
                                                , parserTrace
                                                )
import           Text.ParserCombinators.Parsec
import           Utils                          ( (==>)
                                                , get2022Resources
                                                )

data Crate = Empty
           | Crate Char
           deriving (Show, Eq)

fromCrate (Crate x) = x
parseCrate :: GenParser Char st Crate
parseCrate = do
    char '['
    crate <- anyChar
    char ']'
    return $ Crate crate

parseEmpty :: GenParser Char st Crate
parseEmpty = do
    string "   "
    return Empty


parseNumber :: GenParser Char st ()
parseNumber = do
    char ' '
    digit
    char ' '
    return ()

parseNumbers = many parseNumber


parseCrates = many (choice [parseEmpty, parseCrate] ==> optional (char ' '))
parseCrates' = do
    crates <- manyTill (parseCrates ==> char '\n') (try parseNumber)
    manyTill anyChar (char '\n')
    return $ map (filter (/= Empty)) . transpose $ crates

parseMovements :: GenParser Char st (Int, Int, Int)
parseMovements = do
    string "move "
    quantity <- many digit
    string " from "
    source <- many digit
    string " to "
    dest <- many digit
    return (read quantity, read source - 1, read dest - 1)

popIt items from qty = f items 0
  where
    f [] _ = error "Shoudl happend"
    f xss@(x : xs) i | i == from = (top, bottom : xs)
                     | otherwise = (poped, x : lst)
      where
        (poped, lst   ) = f xs (i + 1)
        (top  , bottom) = splitAt qty x

addIt items to qty = f items 0
  where
    f [] _ = error "Shoudl happend"
    f xss@(x : xs) i | i == to   = (qty ++ x) : xs
                     | otherwise = x : f xs (i + 1)

moveCrate9000 crates (qty, from, to) = addIt crates' to $ reverse popedCrates
    where (popedCrates, crates') = popIt crates from qty

moveCrate9001 crates (qty, from, to) = addIt crates' to popedCrates
    where (popedCrates, crates') = popIt crates from qty

parseSupply = do
    crates <- parseCrates'
    char '\n'
    movements <- many (parseMovements ==> optional (char '\n'))
    return (crates, movements)

partOne str = do
    (crates, moves) <- parse parseSupply "" str
    return $ map (fromCrate . head) $ foldl
        (\cs (q, f, t) -> moveCrate9000 cs (q, f, t))
        crates
        moves

partTwo str = do
    (crates, moves) <- parse parseSupply "" str
    return $ map (fromCrate . head) $ foldl
        (\cs (q, f, t) -> moveCrate9001 cs (q, f, t))
        crates
        moves

example' = intercalate
    "\n"
    [ "    [D]    "
    , "[N] [C]    "
    , "[Z] [M] [P]"
    , " 1   2   3 "
    , ""
    , "move 1 from 2 to 1"
    , "move 3 from 1 to 3"
    , "move 2 from 2 to 1"
    , "move 1 from 1 to 2"
    ]

main = do
    input <- readFile $ get2022Resources 5
    print "Main"
    print $ partOne example'
    print $ partOne input
    print $ partTwo example'
    print $ partTwo input
