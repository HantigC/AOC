module Day03 where
import qualified Utils as U


toBit :: Char -> Int
toBit '1' = 1
toBit '0' = 0
toBit _ = error "Not expected this"


toBits :: [Char] -> [Int]
toBits = map toBit




bitStrToDec :: [Char] -> Int
bitStrToDec = U.binToDec . toBits


partition :: (a -> Bool) -> [a] -> ([a], [a])
partition criteria = foldr select ([], [])
  where
    select x (xs, ys)
      | criteria x = (x:xs, ys)
      | otherwise = (xs, x:ys)


partOne :: [String] -> Int
partOne [] = 0
partOne xn@(x:xs) = o2 * co2
  where
    countBits :: ([Int], Int) -> [Char] -> ([Int], Int)
    countBits (bits, cnt) strBits = (zipWith (+) bits $ map toBit strBits, cnt+1)

    compMajority :: Int -> Int
    compMajority x = if fromIntegral x / fromIntegral totalCnt > 0.5
                        then 1
                        else 0

    (cnts, totalCnt) = foldl countBits (take (length x) [0,0..], 0) xn
    o2 = U.binToDec $ map compMajority cnts
    co2 = 2 ^ length x - 1 - o2


partTwo :: [String] -> Int
partTwo xs = bitStrToDec o2 * bitStrToDec co2
  where
    selectBy :: ([String] -> [String] -> [String]) -> Int -> [String] -> String
    selectBy _ _ [x] = x
    selectBy f ith xs = selectBy f (ith+1) $ f zeros ones
      where
        (zeros, ones) = partition (\str -> str !! ith == '0') xs

    o2 = selectBy (\xs ys -> if length xs > length ys then xs else ys) 0 xs
    co2 = selectBy (\xs ys -> if length xs <= length ys then xs else ys) 0 xs


example = [ "00100"
          , "11110"
          , "10110"
          , "10111"
          , "10101"
          , "01111"
          , "00111"
          , "11100"
          , "10000"
          , "11001"
          , "00010"
          , "01010"
          ]

main = do
  bitStr <- U.readLines "../resources/Day03.txt"
  print $ partOne bitStr
  print $ partTwo bitStr

