module Day06 where

import           Utils                          ( get2022Resources )
example = "nppdvjthqldpwncqszvftbrmjlhg"

detectMarker xss msgLength = f xss [] 0
  where
    f [] window n = n
    f xss@(x : xs) window n
        | length window == msgLength = n
        | x `elem` window            = f xs (lastDistincts ++ [x]) (n + 1)
        | otherwise                  = f xs (window ++ [x]) (n + 1)
        where lastDistincts = tail . dropWhile (x /=) $ window

main = do
    input <- readFile $ get2022Resources 6

    print $ detectMarker example 4
    print $ detectMarker input 4

    print $ detectMarker example 14
    print $ detectMarker input 14
