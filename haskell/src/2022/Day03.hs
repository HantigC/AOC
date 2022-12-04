module Day03 where
import           Data.Char                      ( isUpper
                                                , ord
                                                )
import           Data.List                      ( intersect )
import           Data.List.Split                ( chunksOf )
import           Utils                          ( get2022Resources
                                                , readLines
                                                )

splitHalf xs = splitAt (length xs `div` 2) xs

toPriority c | isUpper c = ord c - ord 'A' + 27
             | otherwise = ord c - ord 'a' + 1

partOne = sum . map (toPriority . head . uncurry intersect . splitHalf)

partTwo = sum . map (toPriority . head . intersect3) . chunksOf 3
    where intersect3 [xs, ys, zs] = xs `intersect` ys `intersect` zs

example =
    [ "vJrwpWtwJgWrhcsFMMfFFhFp"
    , "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
    , "PmmdzqPrVvPwwTWBwg"
    , "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
    , "ttgJtRGJQctTZtZT"
    , "CrZsJsPPZsGzwwsLwLmpwMDw"
    ]

main = do
    lines <- readLines $ get2022Resources 3
    print $ partOne example
    print $ partOne lines


    print $ partTwo example
    print $ partTwo lines
    print "se poate"
