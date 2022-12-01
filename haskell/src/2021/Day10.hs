module Day10 where
import           Control.Monad                  ( foldM
                                                , mapM
                                                )
import           Data.List                      ( sort )
import qualified Data.Map                      as M
import           Utils                         as U

syntax =
  [ "[({(<(())[]>[[{[]{<()<>>"
  , "[(()[<>])]({[<{<<[]>>("
  , "{([(<{}[<>[]}>{[]{[(<()>"
  , "(((({<>}<{<{<>}{[]{[]{}"
  , "[[<[([]))<([[{}[[()]]]"
  , "[{[{({}]{}}([{[{{{}}([]"
  , "{<[[]]>}<{[{[{[]{()[[[]"
  , "[<(<(<(<{}))><([]([]()"
  , "<{([([[(<>()){}]>(<<{{"
  , "<{([{{}}[<[[[<>{}]]]>[]]"
  ]

syntaxCostMap = M.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]


data Syntax a = SyntaxError a
              | SyntaxIncomplete [a]
              | Ok
              | UnknownToken a deriving (Ord, Show, Eq)

instance Functor Syntax where
  fmap f (SyntaxError      x ) = SyntaxError $ f x
  fmap f (SyntaxIncomplete xs) = SyntaxIncomplete $ map f xs
  fmap f Ok                    = Ok
  fmap f (UnknownToken x)      = UnknownToken $ f x


checkSyntax :: [String] -> [Syntax Char]
checkSyntax = map (f [])
 where
  f []    []            = Ok
  f stack []            = SyntaxIncomplete stack
  f stack (s@'(' : str) = f (s : stack) str
  f stack (s@'[' : str) = f (s : stack) str
  f stack (s@'{' : str) = f (s : stack) str
  f stack (s@'<' : str) = f (s : stack) str
  f (ss : stack) (s@'>' : str) =
    if ss == '<' then f stack str else SyntaxError s
  f (ss : stack) (s@']' : str) =
    if ss == '[' then f stack str else SyntaxError s
  f (ss : stack) (s@'}' : str) =
    if ss == '{' then f stack str else SyntaxError s
  f (ss : stack) (s@')' : str) =
    if ss == '(' then f stack str else SyntaxError s
  f _ (s : str) = UnknownToken s

partOne :: [String] -> Maybe Int
partOne strs = sum <$> mapM getScore errornousSyntaxes
 where
  isSyntaxError (SyntaxError _) = True
  isSyntaxError _               = False

  errornousSyntaxes = filter isSyntaxError . checkSyntax $ strs

  getScore (SyntaxError ')') = Just 3
  getScore (SyntaxError ']') = Just 57
  getScore (SyntaxError '}') = Just 1197
  getScore (SyntaxError '>') = Just 25137
  getScore _                 = Nothing


completeSyntax :: [Syntax Char] -> [Maybe String]
completeSyntax []                             = []
completeSyntax ((SyntaxIncomplete xs) : stxs) = f xs : completeSyntax stxs
 where
  f []            = Just []
  f (s@'(' : str) = (')' :) <$> f str
  f (s@'[' : str) = (']' :) <$> f str
  f (s@'{' : str) = ('}' :) <$> f str
  f (s@'<' : str) = ('>' :) <$> f str
  f _             = Nothing
completeSyntax (_ : stxs) = completeSyntax stxs


partTwo :: [String] -> Maybe Int
partTwo strs = do
  completedSyntaxes <- sequence $ completeSyntax incompleteSyntaxes
  scores <- sort <$> mapM (foldl computeScore (Just 0)) completedSyntaxes
  return $ scores !! (length scores `div` 2)
 where
  isSyntaxIncomplete (SyntaxIncomplete _) = True
  isSyntaxIncomplete _                    = False

  incompleteSyntaxes = filter isSyntaxIncomplete . checkSyntax $ strs

  computeScore x y = (+) <$> ((* 5) <$> x) <*> getScore y

  getScore ')' = Just 1
  getScore ']' = Just 2
  getScore '}' = Just 3
  getScore '>' = Just 4
  getScore _   = Nothing

main :: IO ()
main = do
  lines <- U.readLines $ U.get2021Resources 10
  print . partOne $ syntax
  print . partOne $ lines
  print . partTwo $ syntax
  print . partTwo $ lines
