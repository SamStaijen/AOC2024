module DayTwo where

import DayOne ()
import Debug.Trace

--for each list, if elems are all + or all - (not mixed) and step of max 3 per time, then safe
dayTwoPartOne :: [Char] -> Int
dayTwoPartOne fileContent =
  let
    legalLines = length $ filter id $ map ((isLegalDelta . leftRight) . map read . words) (lines fileContent)
  in
    legalLines

leftRight :: [Int] -> [Int]
leftRight xs = zipWith (-) (init xs) (tail xs)

isLegalDelta :: [Int] -> Bool
isLegalDelta xs = all (>= 1) xs && all (<= 3) xs
                || all (<= -1) xs && all (>= -3) xs