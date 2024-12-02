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

--Now, the same rules apply as before, except if removing a single level from an unsafe report would make it safe, 
--the report instead counts as safe.
dayTwoPartTwo :: [Char] -> Int
dayTwoPartTwo fileContent =
  let
    intss = map (map read . words) $ lines fileContent
    listWithRemoved =  map oneRemovedBuilder intss
    listOfValidLines = filter (==True) $ map isLegalWithToleration listWithRemoved
    isLegalWithToleration = any (isLegalDelta . leftRight) --helper func
  in length listOfValidLines

oneRemovedBuilder :: [Int] -> [[Int]]
oneRemovedBuilder xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]