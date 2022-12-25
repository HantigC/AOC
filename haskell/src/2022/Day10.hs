module Day10 where

import Control.Monad.State
import Data.Either (fromRight)
import Data.List (intercalate)
import qualified Text.ParserCombinators.Parsec as P
import Utils (get2022Resources)

data Intruction = Noop | Addx Int deriving (Show, Eq)

parseNoop :: P.GenParser Char st Intruction
parseNoop = do
  P.string "noop"
  P.optional $ P.char '\n'
  return Noop

parseAddx :: P.GenParser Char st Intruction
parseAddx = do
  P.string "addx "
  minus <- P.optionMaybe (P.char '-')
  number <- P.many P.digit
  P.optional $ P.char '\n'
  let negative = case minus of
        Nothing -> read number
        _ -> - (read number)

  return $ Addx negative

type Timer = Int

type CPUState = (Int, Timer, [Int])

parseIntructions :: P.GenParser Char st [Intruction]
parseIntructions = P.many $ parseNoop P.<|> parseAddx

executeIntruction :: Intruction -> State CPUState (Int, [Int])
executeIntruction instruction = case instruction of
  Noop -> state $ f 0 0
  Addx x -> state $ f x 1
  where
    update t v ss =
      if (t - 20) `mod` 40 == 0
        then (t * v) : ss
        else ss

    f s 0 (val, timer, strengths) =
      let newVal = val + s
          nextTimer = timer + 1
          strengths' = update nextTimer val strengths
       in ( (newVal, strengths'),
            (newVal, nextTimer, strengths')
          )
    f s t (val, timer, strengths) =
      let nextTimer = 1 + timer
          strengths' = update nextTimer val strengths
       in f s (t - 1) (val, nextTimer, strengths')

drawInstruction :: Intruction -> State (Int, Int, String) ()
drawInstruction instruction = case instruction of
  Noop -> state $ f 0 0
  Addx x -> state $ f x 1
  where
    update x sprite sts =
      let lowS = sprite `mod` 40 - 1
          highS = sprite `mod` 40 + 1
          newLine = if sprite `mod` 40 == 0 then "\n" else ""
       in if lowS <= x && x <= highS
            then sts ++ newLine ++ "#"
            else sts ++ newLine ++ " "

    f s 0 (val, timer, strengths) =
      let newVal = val + s
          nextTimer = timer + 1
          strengths' = update val timer strengths
       in ((), (newVal, nextTimer, strengths'))
    f s t (val, timer, strengths) =
      let nextTimer = 1 + timer
          strengths' = update val timer strengths
       in f s (t - 1) (val, nextTimer, strengths')

executeInstructions :: [Intruction] -> CPUState -> State CPUState (Int, [Int])
executeInstructions [] s@(v, t, sts) = state $ const ((v, sts), s)
executeInstructions [x] _ = executeIntruction x
executeInstructions (x : xs) s = executeIntruction x >> executeInstructions xs s

executeDrawings [] s = state $ const ((), s)
executeDrawings [x] _ = drawInstruction x
executeDrawings (x : xs) s = drawInstruction x >> executeDrawings xs s

partOne str =
  let instructions = fromRight [] (P.parse parseIntructions "" str)
      (_, _, updates) = execState (executeInstructions instructions (1, 0, [])) (1, 0, [])
   in sum updates

partTwo str = do
  let instructions = fromRight [] (P.parse parseIntructions "" str)
      (_, _, updates) = execState (executeDrawings instructions (1, 0, "")) (1, 0, "")
   in updates

example = intercalate "\n" ["noop", "addx 3", "addx -5"]

main = do
  input <- readFile $ get2022Resources 10
  example <- readFile "./src/2022/day10.in"
  print $ partOne input
  -- putStr $ partTwo example
  putStr $ partTwo input