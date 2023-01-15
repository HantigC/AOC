module Main where

import Utils ((==>))
import Year2021.Day15 (partOne, partTwo)

main :: IO ()
main = do
  result <- partOne
  print result
  result <- partTwo
  print result
