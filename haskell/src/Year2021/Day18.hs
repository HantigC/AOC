module Year2021.Day18 where


import           Data.Char                      ( digitToInt )
import           Text.ParserCombinators.Parsec
import           Utils                          ( (==>)
                                                , get2021Resources
                                                , readLines
                                                )

data JellyNumber a = Value a
                 | Node (JellyNumber a) (JellyNumber a)
                 deriving (Show)


parseJellyNumber :: GenParser Char st (JellyNumber Int)
parseJellyNumber = do
    ch <- anyChar
    case ch of
        '[' -> do
            firstP <- parseJellyNumber
            Node firstP <$> parseJellyNumber
        ']' -> parseJellyNumber
        ',' -> parseJellyNumber
        _   -> return $ Value (digitToInt ch)

parseOneJellyNumber = do
    num <- parseJellyNumber
    optional (char '\n')
    return num

partOne str = mapM (parse parseJellyNumber "") str

main = do
    input <- readLines $ get2021Resources 18
    print $ partOne $ input
