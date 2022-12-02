module Day02 where

import           Utils                          ( get2022Resources
                                                , readLines
                                                )
score "A X" = 3
score "B Y" = 3
score "C Z" = 3


score "A Y" = 6
score "B Z" = 6
score "C X" = 6

score _     = 0

shapeScore 'X' = 1
shapeScore 'Y' = 2
shapeScore 'Z' = 3

shapeScore _   = 0

win 'A' = 'Y'
win 'B' = 'Z'
win 'C' = 'X'


lose 'A' = 'Z'
lose 'B' = 'X'
lose 'C' = 'Y'


draw 'A' = 'X'
draw 'B' = 'Y'
draw 'C' = 'Z'

score' [x, y, 'X'] = [x, y, lose x]
score' [x, y, 'Y'] = [x, y, draw x]
score' [x, y, 'Z'] = [x, y, win x]

sideBySide lines = (scores, shapeScores)
  where
    scores      = map score lines
    shapeScores = map (shapeScore . selectThird) lines
    selectThird = head . tail . tail

partOne lines = sum . zipWith (+) scores $ shapeScores
    where (scores, shapeScores) = sideBySide lines

partTwo lines = partOne $ map score' lines


example = ["A Y", "B X", "C Z"]
main = do
    lines <- readLines $ get2022Resources 2

    print $ partOne example
    print $ partOne lines

    print $ partTwo example
    print $ partTwo lines
