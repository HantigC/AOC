module Day07 where

import           Data.Either                    ( fromRight )
import           Data.List
import           Data.Maybe
import           Data.String                    ( IsString(fromString) )
import           Text.ParserCombinators.Parsec
import           Utils                          ( (==>)
                                                , get2022Resources
                                                )

data FileType = FileDescr {getName :: String, getSize :: Int}
              | DirDescr {getDirname :: String}
              deriving Show

data Command = Ls {getListing :: [FileType]}
             | Cd {getDir :: String}
             deriving Show

data FileSystem = File {getFileName :: String, getFileSize :: Int}
                | Dir { getDirName :: String
                      , getParrent :: Maybe FileSystem
                      , getContent :: [FileSystem]
                      }


prettyPrint :: FileSystem -> Int -> Int -> [Char]
prettyPrint cwd spaces stepSize = f cwd 0
  where
    f (File filename filesize) spaces =
        replicate spaces ' '
            ++ "- "
            ++ filename
            ++ " "
            ++ "("
            ++ "file, size="
            ++ show filesize
            ++ ")"
            ++ "\n"
    f (Dir dirname _ contents) spaces =
        replicate spaces ' '
            ++ "- "
            ++ dirname
            ++ " (dir)"
            ++ "\n"
            ++ concatMap (`f` (spaces + stepSize)) contents


instance Show FileSystem where
    show fs = prettyPrint fs 0 2

fromFileType2FileSystem (FileDescr filename filesize) = File filename filesize
fromFileType2FileSystem (DirDescr dirname           ) = Dir dirname Nothing []

parseLs = do
    string "ls\n"
    Ls <$> many (parseDir <|> parseFile)

parseCd = do
    string "cd "
    Cd <$> manyTill anyChar (char '\n')

parseCommand = do
    string "$ "
    parseLs <|> parseCd

parseDir = do
    string "dir "
    dirname <- manyTill anyChar (char '\n')
    return $ DirDescr dirname

parseFile = do
    filesize <- many digit
    char ' '
    filename <- manyTill anyChar (char '\n')
    return $ FileDescr filename (read filesize)


root = Dir "/" Nothing []
executeCommands ((Cd "/" : commands)) = f commands root
  where
    f (Cd ".." : cmds) x@(Dir _ px@(Just (Dir pn pp contents)) _) =
        f cmds $ Dir pn pp (replace x contents)
    f (Cd toDirname : cmds) dir@(Dir dirname parentDir content) =
        f cmds $ Dir toDirname (Just dir) []
    f (Ls lsOutput : cmds) (Dir dirname parentDir content) =
        f cmds $ Dir dirname parentDir (map fromFileType2FileSystem lsOutput)
    f [] cwd = toRoot cwd
    f _  _   = error "([Char])"
    replace _ [] = []
    replace x@(Dir dirname1 _ _) ((Dir dirname2 _ _) : dirs)
        | dirname1 == dirname2 = x : dirs
    replace y (x : xs) = x : replace y xs

    toRoot x@(Dir _ Nothing _) = x
    toRoot x@(Dir _ px@(Just (Dir pn pp contents)) _) =
        toRoot $ Dir pn pp (replace x contents)
    toRoot _ = error "Not File"
executeCommands _ = error "([Char])"

flattenFileSystem (File _ filesize ) = ([], filesize)
flattenFileSystem (Dir _ _ contents) = (allSize : concatMap fst sizes, allSize)
  where
    sizes   = map flattenFileSystem contents
    allSize = sum . map snd $ sizes

partOne :: String -> Int
partOne =
    sum
        . filter (< 100000)
        . fst
        . flattenFileSystem
        . executeCommands
        . fromRight []
        . parse (many parseCommand) ""


partTwo :: String -> Int
partTwo str = minimum . filter (\x -> total - x <= 40000000) $ sizes
  where
    (sizes, total) =
        flattenFileSystem
            . executeCommands
            . fromRight []
            . parse (many parseCommand) ""
            $ str

example = intercalate
    "\n"
    [ "$ cd /"
    , "$ ls"
    , "dir a"
    , "14848514 b.txt"
    , "8504156 c.dat"
    , "dir d"
    , "$ cd a"
    , "$ ls"
    , "dir e"
    , "29116 f"
    , "2557 g"
    , "62596 h.lst"
    , "$ cd e"
    , "$ ls"
    , "584 i"
    , "$ cd .."
    , "$ cd .."
    , "$ cd d"
    , "$ ls"
    , "4060174 j"
    , "8033020 d.log"
    , "5626152 d.ext"
    , "7214296 k\n"
    ]

main = do
    input <- readFile $ get2022Resources 7

    print $ partOne example
    print $ partOne input


    print $ partTwo example
    print $ partTwo input
